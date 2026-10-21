// @vitest-environment jsdom
// Input forwarding in terminal.ts, driven by synthetic DOM events: the one
// piece of DOM wiring with tests of its own, since the AltGraph class of bug
// lived exactly there (docs/wasm-frontend-unified-plan.md, Ground rules).
// Synthetic events in, forwarded argument tuples and default actions out.
import { describe, it, expect, vi } from "vitest";
import { mountTerminal } from "./terminal.js";

const W = 4;
const H = 3;

function mount() {
  const container = document.createElement("div");
  document.body.appendChild(container);
  const onKey = vi.fn();
  const onWheel = vi.fn();
  const onMouseUp = vi.fn();
  const memory = { buffer: new ArrayBuffer(4 * W * H) } as WebAssembly.Memory;
  const term = mountTerminal(container, () => memory, onKey, onWheel, onMouseUp);
  // The first frame builds the grid whose cells carry the mouse listeners.
  term.submitFrame(0, W, H);
  return { container, onKey, onWheel, onMouseUp };
}

function keydown(target: EventTarget, init: KeyboardEventInit): KeyboardEvent {
  const e = new KeyboardEvent("keydown", { bubbles: true, cancelable: true, ...init });
  target.dispatchEvent(e);
  return e;
}

describe("keydown forwarding", () => {
  it("forwards the key and the four modifier flags", () => {
    const { container, onKey } = mount();
    keydown(container, { key: "a" });
    keydown(container, { key: "B", ctrlKey: true, shiftKey: true, metaKey: true });
    expect(onKey.mock.calls).toEqual([
      ["a", false, false, false, false],
      ["B", true, true, false, true],
    ]);
  });
  it("forwards AltGraph as Alt", () => {
    const { container, onKey } = mount();
    keydown(container, { key: "@", modifierAltGraph: true });
    keydown(container, { key: "x", altKey: true });
    expect(onKey.mock.calls).toEqual([
      ["@", false, false, true, false],
      ["x", false, false, true, false],
    ]);
  });
  it("forwards nothing from outside the container", () => {
    const { onKey } = mount();
    const elsewhere = document.createElement("a");
    document.body.appendChild(elsewhere);
    keydown(elsewhere, { key: "Tab" });
    expect(onKey).not.toHaveBeenCalled();
  });
  it("takes a game key from the browser, Tab among them", () => {
    const { container } = mount();
    expect(keydown(container, { key: "Tab" }).defaultPrevented).toBe(true);
    expect(keydown(container, { key: "a" }).defaultPrevented).toBe(true);
    expect(keydown(container, { key: "a", ctrlKey: true }).defaultPrevented).toBe(true);
  });
  it("leaves the browser zoom, an Alt chord and a dead key", () => {
    const { container } = mount();
    expect(keydown(container, { key: "+", ctrlKey: true }).defaultPrevented).toBe(false);
    expect(keydown(container, { key: "0", ctrlKey: true }).defaultPrevented).toBe(false);
    expect(keydown(container, { key: "a", altKey: true }).defaultPrevented).toBe(false);
    expect(keydown(container, { key: "Shift", shiftKey: true }).defaultPrevented).toBe(false);
  });
});

describe("mouse forwarding", () => {
  it("forwards a wheel turn with its cell and delta, and stops the page scrolling", () => {
    const { container, onWheel } = mount();
    const e = new WheelEvent("wheel", { bubbles: true, cancelable: true, deltaY: -3, shiftKey: true });
    container.children[W + 2].dispatchEvent(e);
    expect(onWheel.mock.calls).toEqual([[2, 1, -3, false, true, false, false]]);
    expect(e.defaultPrevented).toBe(true);
  });
  it("forwards a button release with its cell and DOM button code", () => {
    const { container, onMouseUp } = mount();
    const e = new MouseEvent("mouseup", { bubbles: true, cancelable: true, button: 2, modifierAltGraph: true });
    container.children[2 * W + 3].dispatchEvent(e);
    expect(onMouseUp.mock.calls).toEqual([[3, 2, 2, false, false, true, false]]);
    expect(e.defaultPrevented).toBe(true);
  });
  it("suppresses the browser's context menu", () => {
    const { container } = mount();
    const e = new MouseEvent("contextmenu", { bubbles: true, cancelable: true });
    container.children[0].dispatchEvent(e);
    expect(e.defaultPrevented).toBe(true);
  });
});
