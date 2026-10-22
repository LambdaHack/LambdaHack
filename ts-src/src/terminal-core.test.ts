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
  altModifier,
  cellPosition,
  cellMetrics,
  cellRunOps,
  pointerPoint,
} from "./terminal-core.js";
import fixtures from "./generated/fixtures.json";

// AttrCharW32 layout (Definition/Color.hs): code = w>>16, fg = (w>>8)&0xFF, bg = w&0xFF
const enc = (code: number, fg: number, bg: number) =>
  (code << 16) | (fg << 8) | bg;

// Color enum indices (Definition/Color.hs, data Color)
const Black = 0,
  Green = 2,
  White = 7,
  AltWhite = 8,
  BrBlack = 9,
  BrRed = 10,
  BrCyan = 15,
  BrWhite = 16;
// Highlight enum indices (Definition/Color.hs, data Highlight)
const HiNone = 0,
  HiBackground = 1,
  HiGreen = 2,
  HiRed = 9,
  HiNoneCursor = 13;

const SPACE = 32,
  AT = 64,
  FLOOR = 0xb7 /* '·' */,
  MIDDOT = 0x22c5; /* '⋅' */

describe("decodeCell", () => {
  it("splits glyph/fg/bg out of the Word32", () => {
    expect(decodeCell(enc(AT, BrRed, HiBackground))).toEqual({
      code: AT,
      fg: BrRed,
      bg: HiBackground,
    });
  });
  it("reads fg from the second byte and bg from the low byte", () => {
    expect(decodeCell(enc(0x263a, BrWhite, HiRed))).toEqual({
      code: 0x263a,
      fg: BrWhite,
      bg: HiRed,
    });
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

describe("highlightColor (border, SDL2's chooseAndDrawHighlight rule)", () => {
  it("draws the three non-highlight kinds in black, HighlightBackground included", () => {
    expect(highlightColor(HiNone)).toBe("#000000");
    expect(highlightColor(HiBackground)).toBe("#000000");
    expect(highlightColor(HiNoneCursor)).toBe("#000000");
  });
  it("maps colored highlights through highlightToColor", () => {
    expect(highlightColor(HiRed)).toBe("#D50505");
    expect(highlightColor(HiGreen)).toBe("#059D05");
  });
});

describe("styledCell (full per-cell resolution)", () => {
  it("combines decode + glyph + colors on an odd row", () => {
    expect(styledCell(enc(AT, BrRed, HiNone), 1)).toEqual({
      char: "@",
      color: "#FF5555",
      background: "#000000",
      border: "#000000",
    });
  });
  it("applies the even-row White -> AltWhite tweak to the color", () => {
    expect(styledCell(enc(AT, White, HiNone), 0)).toEqual({
      char: "@",
      color: "#C4BEB1",
      background: "#000000",
      border: "#000000",
    });
  });
  it("substitutes the dim-floor glyph", () => {
    expect(styledCell(enc(FLOOR, White, HiNone), 1).char).toBe(
      String.fromCodePoint(MIDDOT),
    );
  });
  it("uses the highlight tint and border for HighlightBackground", () => {
    const s = styledCell(enc(AT, BrCyan, HiBackground), 1);
    expect(s.background).toBe("#251F1F");
    expect(s.border).toBe("#000000");
  });
});

// The fixtures are generated from CellStyle.styleCell by
// test/CellStyleUnitTests.hs: a mismatch means terminal-core.ts drifted
// from CellStyle.hs.
describe("styledCell against CellStyle.styleCell's fixtures", () => {
  it.each(fixtures.cells)("w=$w row=$row", ({ w, row, ...expected }) => {
    expect(styledCell(w, row)).toEqual(expected);
  });
});

describe("cellPosition against punindex's fixtures", () => {
  it.each(fixtures.positions)(
    "i=$i width=$width",
    ({ i, width, ...expected }) => {
      expect(cellPosition(i, width)).toEqual(expected);
    },
  );
});

describe("cellMetrics (the cell from the loaded font, as Sdl.hs takes it)", () => {
  it("makes a 16px-high font a 16-pixel cell with the line box filling it", () => {
    expect(cellMetrics(13, 3)).toEqual({
      boxSize: 16,
      halfSize: 8,
      baseline: 13,
    });
  });
  it("rounds the font's height up, as TTF_FontHeight does", () => {
    expect(cellMetrics(12.2, 3.1)).toEqual({
      boxSize: 16,
      halfSize: 8,
      baseline: 13,
    });
  });
  it("evens an odd height down, cropping the line box's top row", () => {
    expect(cellMetrics(14, 3)).toEqual({
      boxSize: 16,
      halfSize: 8,
      baseline: 13,
    });
  });
});

describe("cellRunOps (the draw ops of a cell run)", () => {
  const m = cellMetrics(13, 3);
  it("fills, writes and rings each cell in turn, at its column and row", () => {
    expect(
      cellRunOps(
        {
          col: 2,
          row: 1,
          words: [enc(AT, BrRed, HiNone), enc(AT, Green, HiRed)],
        },
        16,
        m,
      ),
    ).toEqual([
      { kind: "fill", x: 32, y: 16, w: 16, h: 16, color: "#000000" },
      {
        kind: "text",
        x: 32,
        y: 16,
        w: 16,
        h: 16,
        baseline: 13,
        text: "@",
        color: "#FF5555",
      },
      { kind: "stroke", x: 32, y: 16, w: 16, h: 16, color: "#000000" },
      { kind: "fill", x: 48, y: 16, w: 16, h: 16, color: "#000000" },
      {
        kind: "text",
        x: 48,
        y: 16,
        w: 16,
        h: 16,
        baseline: 13,
        text: "@",
        color: "#059D05",
      },
      { kind: "stroke", x: 48, y: 16, w: 16, h: 16, color: "#D50505" },
    ]);
  });
  it("lays cells out at the given pitch, a half cell for the mono layer", () => {
    const ops = cellRunOps(
      { col: 3, row: 0, words: [enc(AT, BrRed, HiNone)] },
      m.halfSize,
      m,
    );
    expect(ops.map(({ kind, x, w }) => [kind, x, w])).toEqual([
      ["fill", 24, 8],
      ["text", 24, 8],
      ["stroke", 24, 8],
    ]);
  });
  it("draws a backlit cell's tint and black ring, and no glyph for a space", () => {
    expect(
      cellRunOps(
        { col: 0, row: 0, words: [enc(SPACE, White, HiBackground)] },
        16,
        m,
      ),
    ).toEqual([
      { kind: "fill", x: 0, y: 0, w: 16, h: 16, color: "#251F1F" },
      { kind: "stroke", x: 0, y: 0, w: 16, h: 16, color: "#000000" },
    ]);
  });
  it("styles each cell by its row, as styledCell does", () => {
    const [, text] = cellRunOps(
      { col: 0, row: 2, words: [enc(AT, White, HiNone)] },
      16,
      m,
    );
    expect(text).toMatchObject({
      kind: "text",
      color: styledCell(enc(AT, White, HiNone), 2).color,
    });
  });
});

describe("pointerPoint (pixels to the engine's point, as Sdl.hs's pointTranslate)", () => {
  const m = cellMetrics(13, 3);
  // An 80x24 screen of 16-pixel cells, rendered at its own size.
  const at = (x: number, y: number) =>
    pointerPoint(x, y, 1280, 384, 1280, 384, m);
  it("gives a cell's left half an even x and its right half an odd one", () => {
    expect(at(10 * 16 + 7, 5 * 16)).toEqual({ x: 20, y: 5 });
    expect(at(10 * 16 + 8, 5 * 16 + 15)).toEqual({ x: 21, y: 5 });
  });
  it("scales by the rendered box, as under a CSS transform or devicePixelRatio", () => {
    expect(pointerPoint(10 * 32 + 16, 5 * 32, 2560, 768, 1280, 384, m)).toEqual(
      { x: 21, y: 5 },
    );
    expect(pointerPoint(10 * 8 + 4, 5 * 8, 640, 192, 1280, 384, m)).toEqual({
      x: 21,
      y: 5,
    });
  });
  it("counts the box's far edges as inside it", () => {
    expect(at(1280, 384)).toEqual({ x: 159, y: 23 });
    expect(at(-1, -1)).toEqual({ x: 0, y: 0 });
  });
});

describe("altModifier (AltGr counts as Alt, as in Sdl.hs and Dom.hs)", () => {
  const ev = (altKey: boolean, altGraph: boolean) => ({
    altKey,
    getModifierState: (k: string) => k === "AltGraph" && altGraph,
  });
  it("is true for a plain Alt chord", () => {
    expect(altModifier(ev(true, false))).toBe(true);
  });
  it("is true for an AltGr-only chord, which reports altKey false", () => {
    expect(altModifier(ev(false, true))).toBe(true);
  });
  it("is false with neither held", () => {
    expect(altModifier(ev(false, false))).toBe(false);
  });
});
