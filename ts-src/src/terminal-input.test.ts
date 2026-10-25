// @vitest-environment jsdom
// Input forwarding in terminal.ts, driven by synthetic DOM events: the one
// piece of DOM wiring with tests of its own, since the AltGraph class of bug
// lived exactly there (docs/wasm-frontend-unified-plan.md, Ground rules).
// Synthetic events in, forwarded arguments and default actions out.
import { describe, it, expect, vi, beforeAll } from "vitest";
import { mountTerminal } from "./terminal.js";

const W = 4;
const H = 3;

// jsdom has no 2D context, no font loading and no media queries: stand-ins with
// what terminal.ts reads, a font whose line box is 16 pixels high and so a
// 16-pixel cell.
beforeAll(() => {
  vi.spyOn(HTMLCanvasElement.prototype, "getContext").mockReturnValue({
    measureText: () => ({
      fontBoundingBoxAscent: 13,
      fontBoundingBoxDescent: 3,
    }),
    fillRect() {},
    fillText() {},
    strokeRect() {},
    save() {},
    restore() {},
    beginPath() {},
    rect() {},
    clip() {},
  } as unknown as CanvasRenderingContext2D);
  Object.defineProperty(document, "fonts", {
    value: { load: () => Promise.resolve([]) },
    configurable: true,
  });
  window.matchMedia = () =>
    ({ addEventListener() {} }) as unknown as MediaQueryList;
  vi.stubGlobal("requestAnimationFrame", (f: FrameRequestCallback) => {
    f(0);
    return 1;
  });
});

async function mount() {
  const container = document.createElement("div");
  document.body.appendChild(container);
  const onKey = vi.fn();
  const onWheel = vi.fn();
  const onMouseUp = vi.fn();
  const memory = { buffer: new ArrayBuffer(4 * W * H) } as WebAssembly.Memory;
  const term = mountTerminal(
    container,
    () => memory,
    onKey,
    onWheel,
    onMouseUp,
  );
  // The first frame, painted once the font has loaded, lays out the canvas the
  // pointer is read against: W * 16 by H * 16 pixels, rendered here at twice
  // that.
  term.submitFrame(0, W, H);
  await Promise.resolve();
  await Promise.resolve();
  const canvas = container.querySelector("canvas")!;
  vi.spyOn(canvas, "getBoundingClientRect").mockReturnValue({
    left: 100,
    top: 50,
    width: 2 * W * 16,
    height: 2 * H * 16,
  } as DOMRect);
  return { container, canvas, onKey, onWheel, onMouseUp };
}

function keydown(target: EventTarget, init: KeyboardEventInit): KeyboardEvent {
  const e = new KeyboardEvent("keydown", {
    bubbles: true,
    cancelable: true,
    ...init,
  });
  target.dispatchEvent(e);
  return e;
}

describe("keydown forwarding", () => {
  it("forwards the event itself, for the handler to read", async () => {
    const { container, onKey } = await mount();
    const a = keydown(container, { key: "a" });
    const altGr = keydown(container, { key: "@", modifierAltGraph: true });
    expect(onKey).toHaveBeenCalledTimes(2);
    expect(onKey.mock.calls[0][0]).toBe(a);
    expect(onKey.mock.calls[1][0]).toBe(altGr);
  });
  it("forwards nothing from outside the container", async () => {
    const { onKey } = await mount();
    const elsewhere = document.createElement("a");
    document.body.appendChild(elsewhere);
    keydown(elsewhere, { key: "Tab" });
    expect(onKey).not.toHaveBeenCalled();
  });
  it("leaves each key's default action to the handler", async () => {
    const { container } = await mount();
    expect(keydown(container, { key: "Tab" }).defaultPrevented).toBe(false);
    expect(keydown(container, { key: "a" }).defaultPrevented).toBe(false);
    expect(
      keydown(container, { key: "+", ctrlKey: true }).defaultPrevented,
    ).toBe(false);
  });
});

describe("mouse forwarding", () => {
  // The pointer at a cell's half and row, in the doubled rendering above.
  const at = (halfCol: number, row: number) => ({
    clientX: 100 + halfCol * 16 + 3,
    clientY: 50 + row * 32 + 3,
  });
  it("forwards a wheel turn with its point and delta, and stops the page scrolling", async () => {
    const { canvas, onWheel } = await mount();
    const e = new WheelEvent("wheel", {
      bubbles: true,
      cancelable: true,
      deltaY: -3,
      shiftKey: true,
      ...at(5, 1),
    });
    canvas.dispatchEvent(e);
    expect(onWheel.mock.calls).toEqual([[5, 1, -3, false, true, false, false]]);
    expect(e.defaultPrevented).toBe(true);
  });
  it("leaves a Control wheel turn, the browser's zoom, to the browser", async () => {
    const { canvas, onWheel } = await mount();
    const e = new WheelEvent("wheel", {
      bubbles: true,
      cancelable: true,
      deltaY: -3,
      ctrlKey: true,
      ...at(5, 1),
    });
    canvas.dispatchEvent(e);
    expect(onWheel).not.toHaveBeenCalled();
    expect(e.defaultPrevented).toBe(false);
  });
  it("forwards a button release with its point, in half-cells, and DOM button code", async () => {
    const { canvas, onMouseUp } = await mount();
    const e = new MouseEvent("mouseup", {
      bubbles: true,
      cancelable: true,
      button: 2,
      modifierAltGraph: true,
      ...at(7, 2),
    });
    canvas.dispatchEvent(e);
    expect(onMouseUp.mock.calls).toEqual([
      [7, 2, 2, false, false, true, false],
    ]);
    expect(e.defaultPrevented).toBe(true);
  });
  it("suppresses the browser's context menu", async () => {
    const { canvas } = await mount();
    const e = new MouseEvent("contextmenu", {
      bubbles: true,
      cancelable: true,
    });
    canvas.dispatchEvent(e);
    expect(e.defaultPrevented).toBe(true);
  });
});
