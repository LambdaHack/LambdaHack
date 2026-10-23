// The browser's half of OverlayLayout.hs's overlay layout: the proportional
// pen, which places each chunk of a proportional line by the width the
// renderer measured, re-implemented here and pinned to the Haskell by the
// branch table the test suite writes into generated/prop-fit.json, and the
// draw ops of a frame's three overlays, through terminal-core's draw-op core,
// as Sdl.hs draws them, with the map cells they cover. No DOM, no canvas:
// widths come in as numbers.

import { COLOR, HIGHLIGHT } from "./generated/palette.js";
import {
  cellRunOps,
  decodeCell,
  encodeCell,
  fgColor,
  resolveFg,
  SPACE,
  type CellMetrics,
  type CellRun,
  type DrawOp,
  type Face,
} from "./terminal-core.js";

const DOLLAR = 0x24;

/**
 * A frame's overlays, decoded: OverlayLayout.hs's FrameLayout, its fields
 * in draw order, the section a cell run is in being its layer.
 */
export interface FrameLayout {
  prop: PropLine[];
  square: CellRun[];
  mono: CellRun[];
}

/** One proportional line, OverlayLayout.hs's PropLine. */
export interface PropLine {
  startX: number; // the PointUI x, in half-cells
  row: number;
  chunks: PropChunk[];
}

/**
 * One same-colour run of a proportional line, OverlayLayout.hs's PropChunk,
 * with its face and whether its text is all white space, which the Haskell
 * derives, given.
 */
export interface PropChunk {
  face: "propRegular" | "propBold";
  color: number; // Color index, after the even-row recolour
  allSpace: boolean;
  text: string;
}

/** A frame with no overlays. */
export const NO_OVERLAYS: FrameLayout = { prop: [], square: [], mono: [] };

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
    words: [encodeCell(DOLLAR, COLOR.BrBlack, HIGHLIGHT.HighlightNone)],
  };
}

/**
 * The draw ops of a frame's overlays over a screen width squares wide, in
 * cells of m, in OverlayLayout.hs's draw order and as Sdl.hs draws them:
 * each proportional chunk on black from its left edge, placed and cut by
 * propFit at the ceiling of the width measure gives it, and followed by the
 * trimmed-line marker where propFit raises it; each square run's cells as the
 * map's; and each mono run's cells as monoRunOps draws them. baseline gives a
 * face's baseline below a cell's top. Neither function is called for a frame
 * with no overlays, nor measure for a chunk past the cutoff.
 */
export function overlayOps(
  layout: FrameLayout,
  width: number,
  m: CellMetrics,
  baseline: (face: Face) => number,
  measure: (face: Face, text: string) => number,
): DrawOp[] {
  const lm = layoutMetrics(m.halfSize, width);
  const ops: DrawOp[] = [];
  for (const line of layout.prop) {
    const y = line.row * m.boxSize;
    let cur = startPropLine(lm, line.startX, line.row);
    for (const chunk of line.chunks) {
      if (propCutoff(lm, cur)) break;
      const widthRaw = Math.ceil(measure(chunk.face, chunk.text));
      const fit = propFit(lm, cur, chunk.allSpace, widthRaw);
      const box = { x: fit.x, y, w: fit.widthPx, h: m.boxSize };
      ops.push({ kind: "fill", ...box, color: fgColor(COLOR.Black) });
      ops.push({
        kind: "textLeft",
        ...box,
        baseline: baseline(chunk.face),
        text: chunk.text,
        color: fgColor(chunk.color),
        face: chunk.face,
      });
      if (fit.marker) ops.push(...cellRunOps(fit.marker, m));
      cur = fit.next;
    }
  }
  for (const run of layout.square) ops.push(...cellRunOps(run, m));
  for (const run of layout.mono) ops.push(...monoRunOps(run, m, baseline));
  return ops;
}

// A mono run's draw ops, as Sdl.hs's setMonoChar draws its cells, which
// carry no highlight: a glyph per half cell in the mono face, its colour
// recoloured for the row and its character not substituted, on black and
// with no ring, a space drawing no glyph.
function monoRunOps(
  run: CellRun,
  m: CellMetrics,
  baseline: (face: Face) => number,
): DrawOp[] {
  const ops: DrawOp[] = [];
  const y = run.row * m.boxSize;
  for (let k = 0; k < run.words.length; k++) {
    const x = (run.col + k) * m.halfSize;
    const box = { x, y, w: m.halfSize, h: m.boxSize };
    const { code, fg } = decodeCell(run.words[k]);
    ops.push({ kind: "fill", ...box, color: fgColor(COLOR.Black) });
    if (code !== SPACE) {
      ops.push({
        kind: "text",
        ...box,
        baseline: baseline("mono"),
        text: String.fromCodePoint(code),
        color: fgColor(resolveFg(fg, run.row)),
        face: "mono",
      });
    }
  }
  return ops;
}

/**
 * The cells of a screen cols by rows cells of m that ops draw over,
 * as frame indices in ascending order: those the next frame repaints
 * from its map before drawing its own overlays.
 */
export function overlaidCells(
  ops: DrawOp[],
  m: CellMetrics,
  cols: number,
  rows: number,
): number[] {
  const cells = new Set<number>();
  for (const { x, y, w, h } of ops) {
    const colEnd = Math.min(cols, Math.ceil((x + w) / m.boxSize));
    const rowEnd = Math.min(rows, Math.ceil((y + h) / m.boxSize));
    for (let row = Math.max(0, Math.floor(y / m.boxSize)); row < rowEnd; row++)
      for (
        let col = Math.max(0, Math.floor(x / m.boxSize));
        col < colEnd;
        col++
      )
        cells.add(row * cols + col);
  }
  return [...cells].sort((a, b) => a - b);
}
