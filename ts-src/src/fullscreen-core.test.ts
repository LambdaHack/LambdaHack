import { describe, it, expect } from "vitest";
import { fitTransform, fitTransformCss, toggleLabel } from "./fullscreen-core.js";

describe("fitTransform (aspect-preserving fit, as SDL2's logical size)", () => {
  it("is the identity when the grid already fills the viewport", () => {
    expect(fitTransform(800, 600, 800, 600)).toEqual({ scale: 1, dy: 0 });
  });
  it("is bound by width on a wide grid and centres vertically", () => {
    // 1280x384 grid into 1920x1080: width allows 1.5, height 2.8125.
    expect(fitTransform(1280, 384, 1920, 1080)).toEqual({ scale: 1.5, dy: (1080 - 576) / 2 });
  });
  it("is bound by height on a tall grid, with no vertical slack", () => {
    expect(fitTransform(400, 600, 1920, 1080)).toEqual({ scale: 1.8, dy: 0 });
  });
  it("shrinks a grid larger than the viewport rather than clipping it", () => {
    expect(fitTransform(2000, 500, 1000, 1000).scale).toBe(0.5);
  });
});

describe("fitTransformCss", () => {
  it("translates in px before scaling, so the offset is not scaled", () => {
    expect(fitTransformCss({ scale: 1.5, dy: 252 })).toBe("translateY(252px) scale(1.5)");
  });
});

describe("toggleLabel", () => {
  it("offers the opposite of the current state", () => {
    expect(toggleLabel(false)).toBe("Fullscreen");
    expect(toggleLabel(true)).toBe("Exit fullscreen");
  });
});
