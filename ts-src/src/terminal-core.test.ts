import { describe, it, expect } from "vitest";
import {
  decodeCell,
  isBright,
  resolveFg,
  glyph,
  fgColor,
  bgColor,
  highlightColor,
  styledCell,
} from "./terminal-core.js";

// AttrCharW32 layout (Definition/Color.hs): code = w>>16, fg = (w>>8)&0xFF, bg = w&0xFF
const enc = (code: number, fg: number, bg: number) => (code << 16) | (fg << 8) | bg;

// Color enum indices (Definition/Color.hs, data Color)
const Black = 0, Green = 2, White = 7, AltWhite = 8, BrBlack = 9,
  BrRed = 10, BrCyan = 15, BrWhite = 16;
// Highlight enum indices (Definition/Color.hs, data Highlight)
const HiNone = 0, HiBackground = 1, HiGreen = 2, HiRed = 9;

const SPACE = 32, AT = 64, FLOOR = 0xb7 /* '·' */, MIDDOT = 0x22c5 /* '⋅' */;

describe("decodeCell", () => {
  it("splits glyph/fg/bg out of the Word32", () => {
    expect(decodeCell(enc(AT, BrRed, HiBackground))).toEqual({ code: AT, fg: BrRed, bg: HiBackground });
  });
  it("reads fg from the second byte and bg from the low byte", () => {
    expect(decodeCell(enc(0x263a, BrWhite, HiRed))).toEqual({ code: 0x263a, fg: BrWhite, bg: HiRed });
  });
});

describe("isBright", () => {
  it("is false up to and including BrBlack", () => {
    expect(isBright(White)).toBe(false);
    expect(isBright(AltWhite)).toBe(false);
    expect(isBright(BrBlack)).toBe(false);
  });
  it("is true for BrRed and above", () => {
    expect(isBright(BrRed)).toBe(true);
    expect(isBright(BrWhite)).toBe(true);
  });
});

describe("resolveFg (even-row White -> AltWhite)", () => {
  it("swaps White to AltWhite on even rows", () => {
    expect(resolveFg(White, 0)).toBe(AltWhite);
    expect(resolveFg(White, 2)).toBe(AltWhite);
  });
  it("leaves White unchanged on odd rows", () => {
    expect(resolveFg(White, 1)).toBe(White);
  });
  it("never touches non-White colors", () => {
    expect(resolveFg(BrRed, 0)).toBe(BrRed);
    expect(resolveFg(Green, 2)).toBe(Green);
  });
});

describe("glyph (char substitutions)", () => {
  it("renders a space as a non-breaking space (Dom.hs uses \\x00a0)", () => {
    expect(glyph(SPACE, White)).toBe("\u00a0");
  });
  it("renders a dim floor '·' as the middot '⋅'", () => {
    expect(glyph(FLOOR, White)).toBe(String.fromCodePoint(MIDDOT));
    expect(glyph(FLOOR, BrBlack)).toBe(String.fromCodePoint(MIDDOT));
  });
  it("keeps a bright floor as '·'", () => {
    expect(glyph(FLOOR, BrWhite)).toBe("·");
  });
  it("passes ordinary glyphs through", () => {
    expect(glyph(AT, BrRed)).toBe("@");
  });
});

describe("fgColor (palette)", () => {
  it("maps Color indices to LambdaHack's hex palette", () => {
    expect(fgColor(Black)).toBe("#000000");
    expect(fgColor(White)).toBe("#B8BFCB");
    expect(fgColor(AltWhite)).toBe("#C4BEB1");
    expect(fgColor(BrRed)).toBe("#FF5555");
    expect(fgColor(BrWhite)).toBe("#FFFFFF");
  });
});

describe("bgColor (cell background fill)", () => {
  it("uses the dark tint only for HighlightBackground", () => {
    expect(bgColor(HiBackground)).toBe("#251F1F");
  });
  it("is black for every other highlight", () => {
    expect(bgColor(HiNone)).toBe("#000000");
    expect(bgColor(HiRed)).toBe("#000000");
  });
});

describe("highlightColor (border, via highlightToColor)", () => {
  it("maps HighlightNone to black", () => {
    expect(highlightColor(HiNone)).toBe("#000000");
  });
  it("maps HighlightBackground to BrBlack", () => {
    expect(highlightColor(HiBackground)).toBe("#6F5F5F");
  });
  it("maps colored highlights through highlightToColor", () => {
    expect(highlightColor(HiRed)).toBe("#D50505");
    expect(highlightColor(HiGreen)).toBe("#059D05");
  });
});

describe("styledCell (full per-cell resolution)", () => {
  it("combines decode + glyph + colors on an odd row", () => {
    expect(styledCell(enc(AT, BrRed, HiNone), 1)).toEqual({
      char: "@", color: "#FF5555", background: "#000000", border: "#000000",
    });
  });
  it("applies the even-row White -> AltWhite tweak to the color", () => {
    expect(styledCell(enc(AT, White, HiNone), 0)).toEqual({
      char: "@", color: "#C4BEB1", background: "#000000", border: "#000000",
    });
  });
  it("substitutes the dim-floor glyph", () => {
    expect(styledCell(enc(FLOOR, White, HiNone), 1).char).toBe(String.fromCodePoint(MIDDOT));
  });
  it("uses the highlight tint and border for HighlightBackground", () => {
    const s = styledCell(enc(AT, BrCyan, HiBackground), 1);
    expect(s.background).toBe("#251F1F");
    expect(s.border).toBe("#6F5F5F");
  });
});
