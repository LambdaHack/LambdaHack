// Renderer wiring: a DOM grid of <span> cells, repainted from the engine's
// Word32 frame buffer via the pure styledCell logic. Per-cell diffing skips
// unchanged cells (the engine sends full frames). Not unit-tested (DOM wiring);
// all the decisions live in terminal-core.

import { styledCell } from "./terminal-core.js";

export interface Terminal {
  submitFrame(addr: number, w: number, h: number): void;
}

export type KeyHandler = (
  key: string,
  ctrl: boolean,
  shift: boolean,
  alt: boolean,
  meta: boolean,
) => void;

// col/row are 0-based screen-cell coordinates, matching Dom.hs's per-cell
// mouse handlers (Point{px,py} derived from the cell, not sub-cell offset).
export type WheelHandler = (
  col: number,
  row: number,
  deltaY: number,
  ctrl: boolean,
  shift: boolean,
  alt: boolean,
  meta: boolean,
) => void;

export type MouseHandler = (
  col: number,
  row: number,
  button: number,
  ctrl: boolean,
  shift: boolean,
  alt: boolean,
  meta: boolean,
) => void;

export function mountTerminal(
  container: HTMLElement,
  getMemory: () => WebAssembly.Memory,
  onKey: KeyHandler,
  onWheel: WheelHandler,
  onMouseUp: MouseHandler,
): Terminal {
  let cols = 0;
  let rows = 0;
  let spans: HTMLSpanElement[] = [];
  let prev = new Uint32Array(0);
  let hasFocusedOnce = false;

  function focusTerminal(): void {
    container.focus();
  }

  function buildGrid(w: number, h: number): void {
    cols = w;
    rows = h;
    container.textContent = "";
    container.style.display = "grid";
    container.style.gridTemplateColumns = `repeat(${w}, 1ch)`;
    container.style.gridAutoRows = "1em";
    container.style.fontFamily = "lambdaHackFont, monospace";
    container.style.lineHeight = "1em";
    container.style.whiteSpace = "pre";
    spans = new Array(w * h);
    const frag = document.createDocumentFragment();
    for (let i = 0; i < w * h; i++) {
      const el = document.createElement("span");
      el.style.textAlign = "center";
      const col = i % w;
      const row = Math.floor(i / w);
      // { passive: false } is required to preventDefault() a wheel listener.
      el.addEventListener(
        "wheel",
        (e) => {
          onWheel(col, row, e.deltaY, e.ctrlKey, e.shiftKey, e.altKey, e.metaKey);
          e.preventDefault();
          e.stopPropagation();
        },
        { passive: false },
      );
      el.addEventListener("contextmenu", (e) => {
        // Right-click is delivered via mouseup below, same as Dom.hs.
        e.preventDefault();
        e.stopPropagation();
      });
      el.addEventListener("mouseup", (e) => {
        onMouseUp(col, row, e.button, e.ctrlKey, e.shiftKey, e.altKey, e.metaKey);
        e.preventDefault();
        e.stopPropagation();
      });
      spans[i] = el;
      frag.appendChild(el);
    }
    container.appendChild(frag);
    // Force a full repaint of the new grid.
    prev = new Uint32Array(w * h).fill(0xffffffff);
    if (!hasFocusedOnce) {
      hasFocusedOnce = true;
      // Needed *in addition to* the pageshow listener below, not instead of
      // it: main() is async (fetches/instantiates the wasm module before
      // ever reaching mountTerminal), so on a fresh load the browser's own
      // load/pageshow events can fire and complete before this module gets
      // around to registering its pageshow listener at all -- that race is
      // exactly what left fresh-load/reload without focus. This call, tied
      // to the first real frame actually being ready, doesn't have that
      // race. pageshow still covers bfcache-restored back/forward
      // navigation, which never reaches this buildGrid path again since
      // grid dimensions haven't changed and no setup code reruns.
      focusTerminal();
    }
  }

  let pendingFrame: Uint32Array | null = null;
  let rafHandle: number | null = null;

  // Apply one already-snapshotted frame's cell diffs to the DOM. Deferred
  // to requestAnimationFrame by submitFrame() below, batching same-tick
  // calls into a single browser paint, mirroring Dom.hs's
  // requestAnimationFrame_/newRequestAnimationFrameCallbackSync.
  function applyFrame(buf: Uint32Array): void {
    for (let i = 0; i < buf.length; i++) {
      if (buf[i] === prev[i]) continue;
      prev[i] = buf[i];
      const s = styledCell(buf[i], (i / cols) | 0);
      const el = spans[i];
      el.textContent = s.char;
      el.style.color = s.color;
      el.style.backgroundColor = s.background;
      // Highlight square as an inset outline: invisible when it equals the bg.
      el.style.boxShadow = `inset 0 0 0 1px ${s.border}`;
    }
  }

  // Snapshots the framebuffer and schedules a render; does not paint
  // synchronously itself (see applyFrame, deferred below via rAF).
  function submitFrame(addr: number, w: number, h: number): void {
    if (w !== cols || h !== rows) buildGrid(w, h);
    // Snapshot synchronously: the wasm buffer at `addr` is only valid for
    // the duration of this call (Wasm.hs's display uses an `unsafe` FFI
    // import specifically so the GC can't move/reuse it mid-call). Reading
    // it from a later rAF callback would risk reading stale/reused memory,
    // so only the DOM-mutation work in applyFrame is deferred, not the read.
    pendingFrame = new Uint32Array(getMemory().buffer, addr, w * h).slice();
    if (rafHandle === null) {
      rafHandle = requestAnimationFrame(() => {
        rafHandle = null;
        const frame = pendingFrame;
        pendingFrame = null;
        if (frame) applyFrame(frame);
      });
    }
  }

  // Keys a Ctrl-only chord should still pass to the browser for, mirroring
  // Dom.hs's `browserKeys = "+-0tTnNdxcv"` allowlist: zoom, tab/window
  // management, bookmark, clipboard. Both cases of t/n are listed there (and
  // ported here) to also cover Caps Lock, not Shift -- the modifier check
  // below requires a bare Ctrl chord, so Ctrl+Shift+T doesn't qualify.
  const CTRL_PASSTHROUGH_KEYS = new Set([
    "+", "-", "0", "t", "T", "n", "N", "d", "x", "c", "v",
  ]);

  // Verified against Key.hs's keyTranslateWeb: every KeyboardEvent.key value
  // its DeadKey clauses cover -- chiefly, any modifier key pressed by itself
  // (Shift, Control, Alt/AltGraph, Meta, CapsLock, NumLock, Win,
  // Menu/ContextMenu) plus the literal "Dead" compose-key event.
  const DEAD_KEYS = new Set([
    "Dead", "Shift", "Control", "Meta", "Menu", "ContextMenu",
    "Alt", "AltGraph", "Num_Lock", "NumLock", "Caps_Lock", "CapsLock", "Win",
  ]);

  // Clicking anywhere on the grid (re)focuses the container itself: <span>
  // cells aren't independently focusable, and a click on one doesn't
  // automatically move DOM focus onto its parent. Without this, a mouse
  // click on the grid wouldn't actually route subsequent keydowns here,
  // now that the listener below is scoped to container instead of window.
  container.addEventListener("mousedown", focusTerminal);

  // Covers back/forward navigation restored from bfcache -- e.g. clicking a
  // banner link then going back -- which the buildGrid focus call above
  // can't catch, since bfcache restoration reuses this module's existing
  // state without rerunning any setup code. In principle pageshow also
  // fires on a fresh load, but relying on that turned out unreliable in
  // practice (main() is async and can race ahead of this listener even
  // being registered yet), which is exactly why the buildGrid call above
  // still exists rather than being replaced by this alone.
  window.addEventListener("pageshow", focusTerminal);

  // Scoped to container, not window, to match Dom.hs's listener being
  // attached to a specific div: keydowns firing while focus is elsewhere
  // on the page (e.g. a banner link) must not reach the game at all -- not
  // just skip preventDefault, but skip onKey too. Tab is a real bound game
  // command (cycle party member, per Content/Input.hs), so without this
  // scoping it would both cycle the party AND move page focus at the same
  // time whenever pressed outside the game area, which is wrong; this
  // scoping fixes that for every key uniformly, not just Tab specifically.
  container.addEventListener("keydown", (e) => {
    onKey(e.key, e.ctrlKey, e.shiftKey, e.altKey, e.metaKey);

    const ctrlOnly = e.ctrlKey && !e.shiftKey && !e.altKey && !e.metaKey;
    const altOnly = e.altKey && !e.ctrlKey && !e.shiftKey && !e.metaKey;
    const isDeadKey = DEAD_KEYS.has(e.key);

    const passThrough =
      altOnly || (ctrlOnly && CTRL_PASSTHROUGH_KEYS.has(e.key)) || isDeadKey;

    if (!passThrough) {
      e.preventDefault();
      e.stopPropagation();
    }
  });

  return { submitFrame };
}
