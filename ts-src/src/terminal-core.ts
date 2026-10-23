// Pure logic for the LambdaHack wasm web frontend: turn the engine's raw
// AttrCharW32 cell words into glyphs + colors and runs of them into draw
// ops, each text in a face, a face's ascent and descent into its baseline
// in a cell, frame indices into screen cells, pointer pixels into the
// engine's points, and a pointer event's modifiers into the engine's Alt. No
// DOM, no wasm: the loader and the actual drawing are thin wiring around
// these functions.
//
// Encoding (see LambdaHack's Definition/Color.hs attrCharToW32):
//   char codepoint = w >>> 16 ; fg = (w >>> 8) & 0xFF ; bg = w & 0xFF
// fg indexes the Color enum, bg indexes the Highlight enum.

import {
  BACKLIGHT_FILL,
  COLOR,
  DIM_FLOOR,
  HIGHLIGHT,
  HIGHLIGHT_TO_COLOR,
  PALETTE,
} from "./generated/palette.js";

export interface Cell {
  code: number; // glyph codepoint
  fg: number; // Color enum index
  bg: number; // Highlight enum index
}

export interface StyledCell {
  char: string;
  color: string; // foreground hex
  background: string; // cell background-fill hex
  border: string | null; // highlight border hex, null for none
}

const FLOOR_SYMBOL = 0xb7; // U+00B7, Content/TileKind.hs floorSymbol
export const SPACE = 0x20;

/** Split a Word32 cell into glyph codepoint and color/highlight indices. */
export function decodeCell(w: number): Cell {
  return { code: w >>> 16, fg: (w >>> 8) & 0xff, bg: w & 0xff };
}

/** The Word32 cell of a glyph codepoint and color/highlight indices. */
export function encodeCell(code: number, fg: number, bg: number): number {
  return (code << 16) | (fg << 8) | bg;
}

/**
 * The screen column and row of a frame index, given the screen width,
 * as Haskell's punindex decodes it.
 */
export function cellPosition(
  i: number,
  width: number,
): { col: number; row: number } {
  return { col: i % width, row: Math.floor(i / width) };
}

/** Whether a Color index is "bright" (BrRed..BrWhite), mirroring isBright. */
export function isBright(fg: number): boolean {
  return fg > COLOR.BrBlack;
}

/** Even-row hack: plain White is shown as AltWhite on even rows (Dom.hs). */
export function resolveFg(fgRaw: number, row: number): number {
  return row % 2 === 0 && fgRaw === COLOR.White ? COLOR.AltWhite : fgRaw;
}

/** Glyph to draw, applying the web frontend's char substitution. */
export function glyph(code: number, fg: number): string {
  if (code === FLOOR_SYMBOL && !isBright(fg)) return DIM_FLOOR;
  return String.fromCodePoint(code);
}

/** Foreground color hex for a Color index. */
export function fgColor(fg: number): string {
  return PALETTE[fg];
}

/** Cell background-fill hex for a Highlight index. */
export function bgColor(bg: number): string {
  return bg === HIGHLIGHT.HighlightBackground
    ? BACKLIGHT_FILL
    : PALETTE[COLOR.Black];
}

/**
 * Highlight border hex for a Highlight index, or null for none. SDL2
 * (Sdl.hs's chooseAndDrawHighlight) outlines a cell in highlightToColor,
 * but not the three non-highlight kinds; the Dom.hs border-color write
 * of highlightToColor for every kind, which put a BrBlack ring around
 * each backlit cell, was an artifact of that frontend's table CSS.
 */
export function highlightColor(bg: number): string | null {
  if (
    bg === HIGHLIGHT.HighlightNone ||
    bg === HIGHLIGHT.HighlightBackground ||
    bg === HIGHLIGHT.HighlightNoneCursor
  ) {
    return null;
  }
  return PALETTE[HIGHLIGHT_TO_COLOR[bg]];
}

/**
 * The Alt modifier as the engine wants it: Sdl.hs folds AltGr into Alt
 * (modTranslate) and Dom.hs ORs getAltKey with getAltGraphKey, so a
 * chord held with AltGr alone, the usual case on European layouts, is
 * an Alt chord. Takes the event's two readings rather than the event,
 * so it is testable without a DOM.
 */
export function altModifier(e: {
  altKey: boolean;
  getModifierState(key: string): boolean;
}): boolean {
  return e.altKey || e.getModifierState("AltGraph");
}

/** Fully resolve a cell word at a given screen row into what to draw. */
export function styledCell(w: number, row: number): StyledCell {
  const { code, fg, bg } = decodeCell(w);
  const efg = resolveFg(fg, row);
  return {
    char: glyph(code, efg),
    color: fgColor(efg),
    background: bgColor(bg),
    border: highlightColor(bg),
  };
}

/**
 * A typeface the screen is drawn in: the map font, or one of the chosen
 * fontset's three others, which the startup payload names and Sdl.hs draws
 * the overlays in.
 */
export type Face = "map" | "propRegular" | "propBold" | "mono";

/**
 * The map font's cell in device pixels, as Sdl.hs takes it from the loaded
 * font: halfSize is half the font's height rounded down and boxSize twice
 * that. baseline is the font's faceBaseline in that cell, an odd height
 * cropping a row off the top.
 */
export interface CellMetrics {
  boxSize: number;
  halfSize: number;
  baseline: number;
}

/** CellMetrics from the loaded font's ascent and descent, in device pixels. */
export function cellMetrics(ascent: number, descent: number): CellMetrics {
  const height = Math.ceil(ascent + descent);
  const halfSize = Math.floor(height / 2);
  const boxSize = 2 * halfSize;
  return {
    boxSize,
    halfSize,
    baseline: faceBaseline(boxSize, ascent, descent),
  };
}

/**
 * A face's baseline below the top of a cell boxSize device pixels high,
 * given the loaded face's ascent and descent: its line box centred
 * in the cell as Sdl.hs's scaleSurfaceToTexture centres a glyph's surface,
 * the odd row of a taller box cropped off the top and that of a shorter one
 * left below it.
 */
export function faceBaseline(
  boxSize: number,
  ascent: number,
  descent: number,
): number {
  const height = Math.ceil(ascent + descent);
  return Math.floor((boxSize - height) / 2) + Math.ceil(ascent);
}

/**
 * One canvas drawing command over a rectangle, in device pixels: filling
 * it; a glyph centred in it, its baseline that far below its top, cropped
 * to it as Sdl.hs's scaleSurfaceToTexture crops a glyph to its cell; a run
 * of glyphs from its left edge, cropped to it likewise, as Sdl.hs's
 * scaleSurfaceToTextureProp crops a proportional chunk; or a one-pixel ring
 * along its inner edge, which is SDL2's drawRect. Text is drawn in a face.
 */
export type DrawOp =
  | {
      kind: "fill" | "stroke";
      x: number;
      y: number;
      w: number;
      h: number;
      color: string;
    }
  | {
      kind: "text" | "textLeft";
      x: number;
      y: number;
      w: number;
      h: number;
      baseline: number;
      text: string;
      color: string;
      face: Face;
    };

/** Consecutive cells of one screen row, from column col on, as frame words. */
export interface CellRun {
  col: number;
  row: number;
  words: ArrayLike<number>;
}

/**
 * The draw ops for a cell run of the map's square cells: per cell, its
 * background, its glyph in the map font and its highlight ring, in the order
 * Sdl.hs draws them. A blank glyph draws nothing and gets no op, and a cell
 * without a ring gets none for it.
 */
export function cellRunOps(run: CellRun, m: CellMetrics): DrawOp[] {
  const ops: DrawOp[] = [];
  const y = run.row * m.boxSize;
  for (let k = 0; k < run.words.length; k++) {
    const x = (run.col + k) * m.boxSize;
    const box = { x, y, w: m.boxSize, h: m.boxSize };
    const s = styledCell(run.words[k], run.row);
    ops.push({ kind: "fill", ...box, color: s.background });
    if (decodeCell(run.words[k]).code !== SPACE) {
      ops.push({
        kind: "text",
        ...box,
        baseline: m.baseline,
        text: s.char,
        color: s.color,
        face: "map",
      });
    }
    if (s.border !== null) {
      ops.push({ kind: "stroke", ...box, color: s.border });
    }
  }
  return ops;
}

/**
 * The engine's point under the pointer, as Sdl.hs's pointTranslate gives
 * it: x in half-cells and y in cells. Takes the pointer's offset from
 * the canvas's rendered box and that box's size, which a CSS transform
 * scales, and the backing store's size, so it holds under 1.3's fullscreen
 * fit and at any devicePixelRatio. A pointer on the box's far edge counts
 * as inside it.
 */
export function pointerPoint(
  offsetX: number,
  offsetY: number,
  boxWidth: number,
  boxHeight: number,
  canvasWidth: number,
  canvasHeight: number,
  m: CellMetrics,
): { x: number; y: number } {
  const clamp = (v: number, n: number) => Math.min(Math.max(v, 0), n - 1);
  return {
    x: clamp(
      Math.floor((offsetX * canvasWidth) / boxWidth / m.halfSize),
      canvasWidth / m.halfSize,
    ),
    y: clamp(
      Math.floor((offsetY * canvasHeight) / boxHeight / m.boxSize),
      canvasHeight / m.boxSize,
    ),
  };
}
