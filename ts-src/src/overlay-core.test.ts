import { describe, it, expect, vi } from "vitest";
import {
  NO_OVERLAYS,
  decodeFrameLayout,
  layoutMetrics,
  overlaidCells,
  overlayOps,
  propCutoff,
  propFit,
  type FrameLayout,
  type PropChunk,
} from "./overlay-core.js";
import {
  cellMetrics,
  cellRunOps,
  encodeCell as enc,
  type DrawOp,
  type Face,
} from "./terminal-core.js";
import { COLOR, HIGHLIGHT, PALETTE } from "./generated/palette.js";
import propFitTable from "./generated/prop-fit.json";
import overlayWire from "./generated/overlay-wire.json";

const { Black, White, AltWhite, BrBlack, BrRed, BrCyan } = COLOR;
const {
  HighlightNone: HiNone,
  HighlightRed: HiRed,
  HighlightNoneCursor: HiNoneCursor,
} = HIGHLIGHT;

const SPACE = 32,
  AT = 64,
  FLOOR = 0xb7; /* U+00B7 */

// The cases are generated from OverlayLayout.encodeFrameLayout by
// test/OverlayLayoutUnitTests.hs: a mismatch means decodeFrameLayout drifted
// from OverlayLayout.hs.
describe("decodeFrameLayout against OverlayLayout's wire fixtures", () => {
  it.each(overlayWire.cases)("decodes $name", (c) => {
    expect(decodeFrameLayout(c.words, c.width, c.width)).toEqual(c.layout);
  });
  const fixture = (name: string) =>
    overlayWire.cases.find((c) => c.name === name)!;
  const all = fixture("all three");
  it("refuses overlays laid out for another width than the frame's", () => {
    expect(() =>
      decodeFrameLayout(all.words, all.width, all.width + 1),
    ).toThrow("laid out for 10 squares, a frame 11");
  });
  it("refuses words cut short, or running on past the overlays", () => {
    const { words, width } = all;
    expect(() => decodeFrameLayout(words.slice(0, -1), width, width)).toThrow(
      "no word",
    );
    expect(() => decodeFrameLayout([...words, 0], width, width)).toThrow(
      "1 words past the overlays",
    );
  });
  it("refuses a face or an all-space word that is neither 0 nor 1", () => {
    // The words of the first chunk, after the counts and the line's start.
    const at = (k: number, w: number) =>
      all.words.map((v, j) => (j === k ? w : v));
    expect(() => decodeFrameLayout(at(6, 2), all.width, all.width)).toThrow(
      "word 6 is 2",
    );
    expect(() => decodeFrameLayout(at(8, 2), all.width, all.width)).toThrow(
      "word 8 is 2",
    );
  });
  it("decodes no overlays to a layout that draws and measures nothing", () => {
    const none = fixture("no overlays");
    const measure = vi.fn(() => 0);
    const layout = decodeFrameLayout(none.words, none.width, none.width);
    const ops = overlayOps(
      layout,
      none.width,
      cellMetrics(13, 3),
      () => 13,
      measure,
    );
    expect(ops).toEqual([]);
    expect(measure).not.toHaveBeenCalled();
  });
});

// The rows are generated from OverlayLayout.propCutoff and propFit by
// test/OverlayLayoutUnitTests.hs: a mismatch means overlay-core.ts drifted from
// OverlayLayout.hs.
describe("propCutoff and propFit against OverlayLayout's branch table", () => {
  it.each(propFitTable.rows)(
    "halfSize=$halfSize width=$width cursorX=$cursorX" +
      " allSpace=$allSpace widthRaw=$widthRaw",
    (row) => {
      const lm = layoutMetrics(row.halfSize, row.width);
      const cur = { xPx: row.cursorX, row: 0 };
      expect(propCutoff(lm, cur)).toBe(row.stop);
      expect("x" in row).toBe(!row.stop);
      if (!row.stop) {
        const fit = propFit(lm, cur, row.allSpace, row.widthRaw);
        expect({ x: fit.x, widthPx: fit.widthPx, marker: fit.marker }).toEqual({
          x: row.x,
          widthPx: row.widthPx,
          marker: row.marker,
        });
        // The pen moves on by the width drawn.
        expect(fit.next).toEqual({ xPx: fit.x + fit.widthPx, row: 0 });
      }
    },
  );
});

describe("overlayOps (the draw ops of a frame's overlays)", () => {
  // 16-pixel cells, the map font's baseline 13 and every other face's a
  // different one.
  const m = cellMetrics(13, 3);
  const baseline = (face: Face) =>
    ({ map: 13, propRegular: 12, propBold: 11, mono: 10 })[face];
  const chunk = (text: string, rest: Partial<PropChunk> = {}): PropChunk => ({
    face: "propRegular",
    color: BrRed,
    allSpace: false,
    text,
    ...rest,
  });
  const propOnly = (startX: number, row: number, chunks: PropChunk[]) => ({
    prop: [{ startX, row, chunks }],
    square: [],
    mono: [],
  });

  it("draws nothing for a frame with no overlays, measuring nothing", () => {
    const measure = vi.fn(() => 0);
    const baselineSpy = vi.fn(baseline);
    expect(overlayOps(NO_OVERLAYS, 80, m, baselineSpy, measure)).toEqual([]);
    expect(measure).not.toHaveBeenCalled();
    expect(baselineSpy).not.toHaveBeenCalled();
  });

  it(
    "draws a chunk on black from its left edge," +
      " as wide as measured rounded up",
    () => {
      expect(
        overlayOps(propOnly(3, 1, [chunk("Hi")]), 80, m, baseline, () => 20.2),
      ).toEqual([
        { kind: "fill", x: 24, y: 16, w: 21, h: 16, color: PALETTE[Black] },
        {
          kind: "textLeft",
          x: 24,
          y: 16,
          w: 21,
          h: 16,
          baseline: 12,
          text: "Hi",
          color: PALETTE[BrRed],
          face: "propRegular",
        },
      ]);
    },
  );

  it("starts each chunk where the last ended, measured in its own face", () => {
    const measure = vi.fn((_face: Face, text: string) =>
      text === "a" ? 10 : 30,
    );
    const layout = propOnly(0, 0, [
      chunk("a"),
      chunk("b", { face: "propBold", color: BrCyan }),
    ]);
    const ops = overlayOps(layout, 80, m, baseline, measure);
    expect(ops.map((op) => [op.kind, op.x, op.w])).toEqual([
      ["fill", 0, 10],
      ["textLeft", 0, 10],
      ["fill", 10, 30],
      ["textLeft", 10, 30],
    ]);
    expect(ops[3]).toMatchObject({ face: "propBold", color: PALETTE[BrCyan] });
    expect(measure.mock.calls).toEqual([
      ["propRegular", "a"],
      ["propBold", "b"],
    ]);
  });

  it(
    "cuts an overrun a cell short, marks the last column" +
      " and drops the rest",
    () => {
      // A screen four squares, 64 pixels, wide, the chunk starting at 16.
      const measure = vi.fn(() => 100);
      const layout = propOnly(2, 1, [chunk("long"), chunk("dropped")]);
      expect(overlayOps(layout, 4, m, baseline, measure)).toEqual([
        { kind: "fill", x: 16, y: 16, w: 32, h: 16, color: PALETTE[Black] },
        {
          kind: "textLeft",
          x: 16,
          y: 16,
          w: 32,
          h: 16,
          baseline: 12,
          text: "long",
          color: PALETTE[BrRed],
          face: "propRegular",
        },
        { kind: "fill", x: 48, y: 16, w: 16, h: 16, color: PALETTE[Black] },
        {
          kind: "text",
          x: 48,
          y: 16,
          w: 16,
          h: 16,
          baseline: 13,
          text: "$",
          color: PALETTE[BrBlack],
          face: "map",
        },
      ]);
      expect(measure).toHaveBeenCalledTimes(1);
    },
  );

  it("draws a square run's cells as the map's", () => {
    const run = {
      col: 5,
      row: 2,
      words: [enc(AT, BrRed, HiRed), enc(SPACE, White, HiNone)],
    };
    expect(
      overlayOps(
        { prop: [], square: [run], mono: [] },
        80,
        m,
        baseline,
        () => 0,
      ),
    ).toEqual(cellRunOps(run, m));
  });

  it(
    "draws a mono run a glyph per half cell, recoloured for the row only," +
      " on black with no ring",
    () => {
      const run = {
        col: 3,
        row: 2,
        words: [
          enc(AT, White, HiNoneCursor),
          enc(SPACE, White, HiNone),
          enc(FLOOR, BrBlack, HiNone),
        ],
      };
      expect(
        overlayOps(
          { prop: [], square: [], mono: [run] },
          80,
          m,
          baseline,
          () => 0,
        ),
      ).toEqual([
        { kind: "fill", x: 24, y: 32, w: 8, h: 16, color: PALETTE[Black] },
        {
          kind: "text",
          x: 24,
          y: 32,
          w: 8,
          h: 16,
          baseline: 10,
          text: "@",
          color: PALETTE[AltWhite],
          face: "mono",
        },
        { kind: "fill", x: 32, y: 32, w: 8, h: 16, color: PALETTE[Black] },
        { kind: "fill", x: 40, y: 32, w: 8, h: 16, color: PALETTE[Black] },
        {
          kind: "text",
          x: 40,
          y: 32,
          w: 8,
          h: 16,
          baseline: 10,
          text: "·",
          color: PALETTE[BrBlack],
          face: "mono",
        },
      ]);
    },
  );

  it(
    "draws the proportional lines, then the square runs," +
      " then the mono ones",
    () => {
      const cell = [enc(AT, BrRed, HiNone)];
      const layout: FrameLayout = {
        prop: [{ startX: 0, row: 0, chunks: [chunk("x")] }],
        square: [{ col: 0, row: 0, words: cell }],
        mono: [{ col: 0, row: 0, words: cell }],
      };
      const ops = overlayOps(layout, 80, m, baseline, () => 8);
      expect(ops.flatMap((op) => ("face" in op ? [op.face] : []))).toEqual([
        "propRegular",
        "map",
        "mono",
      ]);
    },
  );
});

describe("overlaidCells (the map cells overlays draw over)", () => {
  const m = cellMetrics(13, 3);
  const black = PALETTE[Black];
  it("is every cell an op reaches into, once each, in index order", () => {
    const ops: DrawOp[] = [
      { kind: "stroke", x: 32, y: 16, w: 16, h: 16, color: black },
      { kind: "fill", x: 24, y: 16, w: 21, h: 16, color: black },
    ];
    expect(overlaidCells(ops, m, 10, 3)).toEqual([11, 12]);
  });
  it("is cut to the screen", () => {
    const ops: DrawOp[] = [
      { kind: "fill", x: 144, y: 32, w: 40, h: 32, color: black },
    ];
    expect(overlaidCells(ops, m, 10, 3)).toEqual([29]);
  });
  it("is nothing for no ops", () => {
    expect(overlaidCells([], m, 10, 3)).toEqual([]);
  });
});
