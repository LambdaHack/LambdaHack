import { describe, it, expect } from "vitest";
import {
  fitTransform,
  fitTransformCss,
  toggleLabel,
} from "./fullscreen-core.js";

describe("fitTransform (aspect-preserving fit, as SDL2's logical size)", () => {
  it("is the identity when the grid already fills the viewport", () => {
    expect(fitTransform(800, 600, 800, 600, 0)).toEqual({
      scale: 1,
      dx: 0,
      dy: 0,
    });
  });
  it("is bound by width on a wide grid and centres vertically", () => {
    // 1280x384 grid into 1920x1080: width allows 1.5, height 2.8125;
    // the grid's auto margins put its box at 320.
    expect(fitTransform(1280, 384, 1920, 1080, 320)).toEqual({
      scale: 1.5,
      dx: -320,
      dy: (1080 - 576) / 2,
    });
  });
  it("is bound by height on a tall grid, with no vertical slack", () => {
    expect(fitTransform(400, 600, 1920, 1080, 760)).toEqual({
      scale: 1.8,
      dx: -160,
      dy: 0,
    });
  });
  it("shrinks a grid larger than the viewport rather than clipping it", () => {
    expect(fitTransform(2000, 500, 1000, 1000, 0).scale).toBe(0.5);
  });
  it("centres the scaled grid wherever its box starts", () => {
    // Auto margins centre a grid narrower than the viewport and resolve
    // to 0 for one wider, which a scale about the box's centre then
    // leaves off-centre: a 1920 px grid in a 1366 px viewport.
    for (const [gridW, left] of [
      [1920, 0],
      [1280, 43],
      [800, 283],
    ]) {
      const t = fitTransform(gridW, 384, 1366, 768, left);
      expect(left + t.dx + (gridW * t.scale) / 2).toBeCloseTo(1366 / 2);
    }
  });
});

describe("fitTransformCss", () => {
  it("translates in px before scaling, so the offset is not scaled", () => {
    expect(fitTransformCss({ scale: 1.5, dx: -320, dy: 252 })).toBe(
      "translate(-320px, 252px) scale(1.5)",
    );
  });
});

describe("toggleLabel", () => {
  it("offers the opposite of the current state", () => {
    expect(toggleLabel(false)).toBe("Fullscreen");
    expect(toggleLabel(true)).toBe("Exit fullscreen");
  });
});
