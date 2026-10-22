// Renderer wiring: one <canvas>, repainted from the engine's Word32 frame
// buffer through the pure draw-op core (terminal-core's cellRunOps) and
// a thin interpreter of its ops below. Per-cell diffing skips unchanged
// cells (the engine sends full frames). Its input forwarding is tested
// under jsdom (terminal-input.test.ts); the rendering decisions live in
// terminal-core, and the key ones in Haskell.

import {
  XHAIR_CURSOR_HOTSPOT_X,
  XHAIR_CURSOR_HOTSPOT_Y,
  XHAIR_CURSOR_URL,
} from "./generated/cursor.js";
import {
  altModifier,
  cellMetrics,
  cellPosition,
  cellRunOps,
  pointerPoint,
  type CellMetrics,
  type DrawOp,
} from "./terminal-core.js";

export interface Terminal {
  submitFrame(addr: number, w: number, h: number): void;
  printScreen(name: string): void;
  setFontScale(scale: number): void;
}

// Takes the event itself: the handler reads the key and modifiers off it
// and decides whether the browser keeps its default action.
export type KeyHandler = (e: KeyboardEvent) => void;

// x is the 0-based PointUI column, in half-cells as Sdl.hs's pointTranslate
// gives it, and y the 0-based screen row.
export type WheelHandler = (
  x: number,
  y: number,
  deltaY: number,
  ctrl: boolean,
  shift: boolean,
  alt: boolean,
  meta: boolean,
) => void;

export type MouseHandler = (
  x: number,
  y: number,
  button: number,
  ctrl: boolean,
  shift: boolean,
  alt: boolean,
  meta: boolean,
) => void;

// The map font, at allFontsScale 1.0 the 16 CSS pixels the DOM grid drew it
// at, its bitmap's native size, and at any other scale that many times 16, as
// Sdl.hs scales its scalable font.
const FONT_FAMILY = "lambdaHackFont";
const FONT_CSS_PX = 16;

export function mountTerminal(
  container: HTMLElement,
  getMemory: () => WebAssembly.Memory,
  onKey: KeyHandler,
  onWheel: WheelHandler,
  onMouseUp: MouseHandler,
): Terminal {
  let cols = 0;
  let rows = 0;
  let dpr = 0;
  // The engine's allFontsScale, and the one the canvas was laid out at.
  let fontScale = 1;
  let laidOutScale = 0;
  let metrics: CellMetrics | null = null;
  let prev = new Uint32Array(0);
  let lastFrame: Uint32Array | null = null;
  let hasFocusedOnce = false;
  let fontLoaded = false;

  const canvas = document.createElement("canvas");
  canvas.width = 0;
  canvas.height = 0;
  container.appendChild(canvas);
  const ctx = canvas.getContext("2d", { alpha: false })!;
  // SDL2's crosshair pointer, generated from the same art (MouseCursor.hs);
  // the keyword is the fallback where the image fails to load.
  container.style.cursor =
    `url(${XHAIR_CURSOR_URL})` +
    ` ${XHAIR_CURSOR_HOTSPOT_X} ${XHAIR_CURSOR_HOTSPOT_Y}, crosshair`;

  function focusTerminal(): void {
    container.focus();
  }

  // Size the canvas for a w by h frame at the current devicePixelRatio:
  // the backing store in device pixels, cols * boxSize by rows * boxSize,
  // boxSize taken from the map font loaded at FONT_CSS_PX times allFontsScale
  // CSS pixels, and the CSS size that divided by devicePixelRatio.
  function layOut(w: number, h: number): void {
    cols = w;
    rows = h;
    dpr = window.devicePixelRatio;
    laidOutScale = fontScale;
    const px = FONT_CSS_PX * fontScale * dpr;
    const font = `${px}px ${FONT_FAMILY}, monospace`;
    ctx.font = font;
    const tm = ctx.measureText("M");
    const m = cellMetrics(tm.fontBoundingBoxAscent, tm.fontBoundingBoxDescent);
    metrics = m;
    // Resizing the backing store resets the context's state.
    canvas.width = w * m.boxSize;
    canvas.height = h * m.boxSize;
    canvas.style.width = `${canvas.width / dpr}px`;
    canvas.style.height = `${canvas.height / dpr}px`;
    ctx.font = font;
    ctx.textAlign = "center";
    ctx.textBaseline = "alphabetic";
    ctx.lineWidth = 1;
    // Force a full repaint of the new canvas.
    prev = new Uint32Array(w * h).fill(0xffffffff);
    if (!hasFocusedOnce) {
      hasFocusedOnce = true;
      // Needed *in addition to* the pageshow listener below, not instead of
      // it: main() is async (fetches/instantiates the wasm module before
      // ever reaching mountTerminal), so on a fresh load the browser's own
      // load/pageshow events can fire and complete before this module gets
      // around to registering its pageshow listener at all -- that race is
      // exactly what left fresh-load/reload without focus. This call, tied
      // to the first real frame actually being painted, doesn't have that
      // race. pageshow still covers bfcache-restored back/forward
      // navigation, which never reaches this layOut path again since
      // the frame size hasn't changed and no setup code reruns.
      focusTerminal();
    }
  }

  // The thin interpreter of terminal-core's draw ops over the 2D context.
  function draw(ops: DrawOp[]): void {
    for (const op of ops) {
      switch (op.kind) {
        case "fill":
          ctx.fillStyle = op.color;
          ctx.fillRect(op.x, op.y, op.w, op.h);
          break;
        case "text":
          ctx.save();
          ctx.beginPath();
          ctx.rect(op.x, op.y, op.w, op.h);
          ctx.clip();
          ctx.fillStyle = op.color;
          ctx.fillText(op.text, op.x + op.w / 2, op.y + op.baseline);
          ctx.restore();
          break;
        case "stroke":
          // A one-pixel line is centred on its path, so the path runs
          // half a pixel inside the rectangle's edge.
          ctx.strokeStyle = op.color;
          ctx.strokeRect(op.x + 0.5, op.y + 0.5, op.w - 1, op.h - 1);
          break;
      }
    }
  }

  // Paint one already-snapshotted frame's changed cells, each stretch
  // of them within a row as one cell run. Deferred to
  // requestAnimationFrame by submitFrame() below, batching same-tick calls
  // into a single browser paint, mirroring Dom.hs's
  // requestAnimationFrame_/newRequestAnimationFrameCallbackSync.
  function applyFrame(buf: Uint32Array, w: number, h: number): void {
    if (
      w !== cols ||
      h !== rows ||
      window.devicePixelRatio !== dpr ||
      fontScale !== laidOutScale
    )
      layOut(w, h);
    const m = metrics!;
    let i = 0;
    while (i < buf.length) {
      if (buf[i] === prev[i]) {
        i++;
        continue;
      }
      const { col, row } = cellPosition(i, cols);
      const rowEnd = i - col + cols;
      let end = i + 1;
      while (end < rowEnd && buf[end] !== prev[end]) end++;
      const words = buf.subarray(i, end);
      prev.set(words, i);
      draw(cellRunOps({ col, row, words }, m.boxSize, m));
      i = end;
    }
    lastFrame = buf;
  }

  let pendingFrame: { buf: Uint32Array; w: number; h: number } | null = null;
  let rafHandle: number | null = null;
  // Screenshots asked for while a frame was pending, taken after its paint.
  let pendingPrints: string[] = [];

  // Paints wait for the map font: measureText and fillText before it loads
  // would use a fallback face.
  function schedulePaint(): void {
    if (rafHandle !== null || !fontLoaded) return;
    rafHandle = requestAnimationFrame(() => {
      rafHandle = null;
      const frame = pendingFrame;
      pendingFrame = null;
      if (frame) applyFrame(frame.buf, frame.w, frame.h);
      const prints = pendingPrints;
      pendingPrints = [];
      for (const name of prints) savePng(name);
    });
  }

  // Offer the canvas as a PNG download under `name`, the backing store
  // as it is, whatever size CSS shows it at.
  function savePng(name: string): void {
    canvas.toBlob((blob) => {
      if (!blob) return;
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = name;
      document.body.appendChild(a);
      a.click();
      a.remove();
      // Revoked later, not at once: the download the click starts may
      // still be reading it.
      setTimeout(() => URL.revokeObjectURL(url), 60_000);
    }, "image/png");
  }

  // Under --printEachScreen the engine asks for a frame's screenshot right
  // after submitting the frame, before its paint, so a print with a frame
  // pending waits for that paint, or it would save the frame before.
  function printScreen(name: string): void {
    if (pendingFrame) pendingPrints.push(name);
    else savePng(name);
  }

  // Snapshots the framebuffer and schedules a render; does not paint
  // synchronously itself (see applyFrame, deferred via rAF).
  function submitFrame(addr: number, w: number, h: number): void {
    // Snapshot synchronously: the wasm buffer at `addr` is only valid for
    // the duration of this call (Wasm.hs's display uses an `unsafe` FFI
    // import specifically so the GC can't move/reuse it mid-call). Reading
    // it from a later rAF callback would risk reading stale/reused memory,
    // so only the drawing work in applyFrame is deferred, not the read.
    const buf = new Uint32Array(getMemory().buffer, addr, w * h).slice();
    pendingFrame = { buf, w, h };
    schedulePaint();
  }

  document.fonts
    .load(`${FONT_CSS_PX}px ${FONT_FAMILY}`)
    .catch((e: unknown) => console.warn("[lh] map font failed to load", e))
    .finally(() => {
      fontLoaded = true;
      schedulePaint();
    });

  // Browser zoom changes devicePixelRatio: repaint the last frame at the new
  // one, which applyFrame lays the canvas out for, rather than leave
  // the browser to resample it. The query matches the ratio it was made
  // at, so each change is caught once and a new query made.
  function watchPixelRatio(): void {
    window
      .matchMedia(`(resolution: ${window.devicePixelRatio}dppx)`)
      .addEventListener(
        "change",
        () => {
          if (lastFrame && !pendingFrame) {
            pendingFrame = { buf: lastFrame, w: cols, h: rows };
            schedulePaint();
          }
          watchPixelRatio();
        },
        { once: true },
      );
  }
  watchPixelRatio();

  // The engine's point under a mouse event; nothing before the first paint.
  function pointUnder(e: MouseEvent): { x: number; y: number } | null {
    const r = canvas.getBoundingClientRect();
    if (!metrics || r.width === 0 || r.height === 0) return null;
    return pointerPoint(
      e.clientX - r.left,
      e.clientY - r.top,
      r.width,
      r.height,
      canvas.width,
      canvas.height,
      metrics,
    );
  }

  // { passive: false } is required to preventDefault() a wheel listener.
  canvas.addEventListener(
    "wheel",
    (e) => {
      const p = pointUnder(e);
      if (p)
        onWheel(
          p.x,
          p.y,
          e.deltaY,
          e.ctrlKey,
          e.shiftKey,
          altModifier(e),
          e.metaKey,
        );
      e.preventDefault();
      e.stopPropagation();
    },
    { passive: false },
  );
  canvas.addEventListener("contextmenu", (e) => {
    // Right-click is delivered via mouseup below, same as Dom.hs.
    e.preventDefault();
    e.stopPropagation();
  });
  canvas.addEventListener("mouseup", (e) => {
    const p = pointUnder(e);
    if (p)
      onMouseUp(
        p.x,
        p.y,
        e.button,
        e.ctrlKey,
        e.shiftKey,
        altModifier(e),
        e.metaKey,
      );
    e.preventDefault();
    e.stopPropagation();
  });

  // Clicking anywhere on the canvas (re)focuses the container itself:
  // the canvas isn't focusable, and a click on it doesn't move DOM focus
  // onto its parent. Without this, a mouse click on the map wouldn't
  // actually route subsequent keydowns here, the listener below being
  // scoped to container instead of window.
  container.addEventListener("mousedown", focusTerminal);

  // Covers back/forward navigation restored from bfcache -- e.g. clicking a
  // banner link then going back -- which the layOut focus call above
  // can't catch, since bfcache restoration reuses this module's existing
  // state without rerunning any setup code. In principle pageshow also
  // fires on a fresh load, but relying on that turned out unreliable in
  // practice (main() is async and can race ahead of this listener even
  // being registered yet), which is exactly why the layOut call above
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
  // Whether the browser keeps a key's default action is decided in Haskell
  // (InputDecision's decideKey), by the handler, before it returns.
  container.addEventListener("keydown", (e) => onKey(e));

  // The scale the engine sizes its fonts by, from its startup payload; the
  // canvas is laid out at it from the next frame on.
  function setFontScale(scale: number): void {
    fontScale = scale;
  }

  return { submitFrame, printScreen, setFontScale };
}
