import { describe, it, expect } from "vitest";
import { fitScale, toggleLabel } from "./fullscreen-core.js";

describe("fitScale (aspect-preserving fit, as SDL2's logical size)", () => {
  it("is 1 when the grid already fills the viewport", () => {
    expect(fitScale(800, 600, 800, 600)).toBe(1);
  });
  it("is bound by width on a wide grid", () => {
    // 1280x384 into 1920x1080: width allows 1.5, height 2.8125.
    expect(fitScale(1280, 384, 1920, 1080)).toBe(1.5);
  });
  it("is bound by height on a tall grid", () => {
    expect(fitScale(400, 600, 1920, 1080)).toBe(1.8);
  });
  it("leaves an unsized grid unscaled, as fullscreen before the first frame finds it", () => {
    expect(fitScale(0, 0, 1920, 1080)).toBe(1);
  });
  it("shrinks a grid larger than the viewport rather than clipping it", () => {
    expect(fitScale(2000, 500, 1000, 1000)).toBe(0.5);
  });
});

describe("toggleLabel", () => {
  it("offers the opposite of the current state", () => {
    expect(toggleLabel(false)).toBe("Fullscreen");
    expect(toggleLabel(true)).toBe("Exit fullscreen");
  });
});
