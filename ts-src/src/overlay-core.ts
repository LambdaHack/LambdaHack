// The browser's half of OverlayLayout.hs's overlay layout: the proportional
// pen, which places each chunk of a proportional line by the width
// the renderer measured, re-implemented here and pinned to the Haskell
// by the branch table the test suite writes into generated/prop-fit.json.
// No DOM, no canvas: widths come in as numbers.

import type { CellRun } from "./terminal-core.js";

const DOLLAR = 0x24;
const BR_BLACK = 9;
const HIGHLIGHT_NONE = 0;

/**
 * Cell metrics in device pixels, OverlayLayout.hs's LayoutMetrics: halfSize
 * is one PointUI step and the mono pitch, the painter's own, and width
 * the screen's width in squares, the frame's.
 */
export interface LayoutMetrics {
  halfSize: number;
  width: number;
}

/** OverlayLayout.hs's mkLayoutMetrics, its assertion thrown. */
export function layoutMetrics(halfSize: number, width: number): LayoutMetrics {
  if (!(halfSize > 0 && width > 1))
    throw new Error(`layoutMetrics: halfSize ${halfSize}, width ${width}`);
  return { halfSize, width };
}

/** One square cell: the square pitch and the row height of every layer. */
export function boxSize(lm: LayoutMetrics): number {
  return 2 * lm.halfSize;
}

/** The pen of one proportional line, in device pixels. */
export interface PropCursor {
  xPx: number;
  row: number;
}

/** The pen at the start of a line, given in PointUI half-cells. */
export function startPropLine(
  lm: LayoutMetrics,
  startX: number,
  row: number,
): PropCursor {
  return { xPx: startX * lm.halfSize, row };
}

/**
 * Whether the pen has reached the last column, in which case this chunk
 * and every later chunk of the line are dropped. Asked before the chunk
 * is measured.
 */
export function propCutoff(lm: LayoutMetrics, cur: PropCursor): boolean {
  return cur.xPx >= (lm.width - 1) * boxSize(lm);
}

/** Where a measured chunk goes, and how much of it. */
export interface PropFit {
  x: number; // device x to draw at
  widthPx: number; // device width to draw and to clip to
  marker: CellRun | null; // the trimmed-line marker, to draw after the chunk
  next: PropCursor;
}

/**
 * Place one chunk, given whether its text is all white space and its natural
 * width, the ceiling of what measureText gives: OverlayLayout.hs's propFit,
 * its precondition, that propCutoff is false, thrown. A chunk that fits
 * is drawn whole; one that overruns the screen is cut, an all-space chunk
 * at the screen's edge and any other one cell short of it, which takes
 * the trimmed-line marker.
 */
export function propFit(
  lm: LayoutMetrics,
  cur: PropCursor,
  allSpace: boolean,
  widthRaw: number,
): PropFit {
  if (propCutoff(lm, cur) || widthRaw < 0)
    throw new Error(`propFit: pen at ${cur.xPx}, width ${widthRaw}`);
  const box = boxSize(lm);
  const remaining = lm.width * box - cur.xPx;
  const width =
    widthRaw <= remaining ? widthRaw : allSpace ? remaining : remaining - box;
  const trimmed = width !== widthRaw && !allSpace;
  return {
    x: cur.xPx,
    widthPx: width,
    marker: trimmed ? trimmedMarkerRun(lm, cur.row) : null,
    next: { xPx: cur.xPx + width, row: cur.row },
  };
}

/**
 * OverlayLayout.hs's trimmedMarkerRun: Color.hs's trimmedLineAttrW32,
 * a BrBlack '$', in the row's last square column, as a square-layer run.
 */
export function trimmedMarkerRun(lm: LayoutMetrics, row: number): CellRun {
  return {
    col: lm.width - 1,
    row,
    words: [(DOLLAR << 16) | (BR_BLACK << 8) | HIGHLIGHT_NONE],
  };
}
