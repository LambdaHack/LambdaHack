// Pure logic for the LambdaHack wasm web frontend: turn the engine's raw
// AttrCharW32 cell words into glyphs + colors and runs of them into draw
// ops, frame indices into screen cells, pointer pixels into the engine's
// points, and a key event's modifiers into the engine's Alt. No DOM, no
// wasm: the loader and the actual drawing are thin wiring around these
// functions.
//
// Encoding (see LambdaHack's Definition/Color.hs attrCharToW32):
//   char codepoint = w >>> 16 ; fg = (w >>> 8) & 0xFF ; bg = w & 0xFF
// fg indexes the Color enum, bg indexes the Highlight enum.

import { BACKLIGHT_FILL, HIGHLIGHT_TO_COLOR, PALETTE } from "./generated/palette.js";

export interface Cell {
  code: number; // glyph codepoint
  fg: number; // Color enum index
  bg: number; // Highlight enum index
}

export interface StyledCell {
  char: string;
  color: string; // foreground hex
  background: string; // cell background-fill hex
  border: string; // highlight border hex
}

const BLACK = 0;
const WHITE = 7;
const ALT_WHITE = 8;
const BR_BLACK = 9;
const HIGHLIGHT_NONE = 0;
const HIGHLIGHT_BACKGROUND = 1;
const HIGHLIGHT_NONE_CURSOR = 13;
const FLOOR_SYMBOL = 0xb7; // '·', Content/TileKind.hs floorSymbol
const FLOOR_DIM = "⋅"; // '⋅', Dom.hs substitution for dim floor
const NBSP = " "; // Dom.hs renders ' ' as a non-breaking space

/** Split a Word32 cell into glyph codepoint and color/highlight indices. */
export function decodeCell(w: number): Cell {
  return { code: w >>> 16, fg: (w >>> 8) & 0xff, bg: w & 0xff };
}

/**
 * The screen column and row of a frame index, given the screen width,
 * as Haskell's punindex decodes it.
 */
export function cellPosition(i: number, width: number): { col: number; row: number } {
  return { col: i % width, row: Math.floor(i / width) };
}

/** Whether a Color index is "bright" (BrRed..BrWhite), mirroring isBright. */
export function isBright(fg: number): boolean {
  return fg > BR_BLACK;
}

/** Even-row hack: plain White is shown as AltWhite on even rows (Dom.hs). */
export function resolveFg(fgRaw: number, row: number): number {
  return row % 2 === 0 && fgRaw === WHITE ? ALT_WHITE : fgRaw;
}

/** Glyph to draw, applying the web frontend's char substitutions. */
export function glyph(code: number, fg: number): string {
  if (code === 0x20) return NBSP;
  if (code === FLOOR_SYMBOL && !isBright(fg)) return FLOOR_DIM;
  return String.fromCodePoint(code);
}

/** Foreground color hex for a Color index. */
export function fgColor(fg: number): string {
  return PALETTE[fg];
}

/** Cell background-fill hex for a Highlight index. */
export function bgColor(bg: number): string {
  return bg === HIGHLIGHT_BACKGROUND ? BACKLIGHT_FILL : PALETTE[BLACK];
}

/**
 * Highlight border hex for a Highlight index. SDL2 (Sdl.hs's
 * chooseAndDrawHighlight) outlines every cell, but draws the three
 * non-highlight kinds in black -- the erase half of its rectangle
 * workaround -- and only the rest in highlightToColor; the Dom.hs
 * border-color write of highlightToColor for every kind, which put a
 * BrBlack ring around each backlit cell, was an artifact of that
 * frontend's table CSS.
 */
export function highlightColor(bg: number): string {
  if (bg === HIGHLIGHT_NONE || bg === HIGHLIGHT_BACKGROUND || bg === HIGHLIGHT_NONE_CURSOR) {
    return PALETTE[BLACK];
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
 * The map font's cell in device pixels, as Sdl.hs takes it from the loaded
 * font: halfSize is half the font's height rounded down and boxSize twice
 * that. baseline is the glyph's baseline below the cell's top, the font's
 * line box being centred in the cell as Sdl.hs's scaleSurfaceToTexture
 * centres a glyph's surface, cropping an odd row off the top.
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
  return { boxSize, halfSize, baseline: Math.floor((boxSize - height) / 2) + Math.ceil(ascent) };
}

/**
 * One canvas drawing command over a rectangle, in device pixels: filling
 * it; a glyph centred in it, its baseline that far below its top, cropped
 * to it as Sdl.hs's scaleSurfaceToTexture crops a glyph to its cell; or
 * a one-pixel ring along its inner edge, which is SDL2's drawRect.
 */
export type DrawOp =
  | { kind: "fill"; x: number; y: number; w: number; h: number; color: string }
  | {
      kind: "text";
      x: number;
      y: number;
      w: number;
      h: number;
      baseline: number;
      text: string;
      color: string;
    }
  | { kind: "stroke"; x: number; y: number; w: number; h: number; color: string };

/** Consecutive cells of one screen row, from column col on, as frame words. */
export interface CellRun {
  col: number;
  row: number;
  words: ArrayLike<number>;
}

/**
 * The draw ops for a cell run whose cells are pitch device pixels wide:
 * per cell, its background, its glyph and its highlight ring, in the order
 * Sdl.hs draws them. A blank glyph draws nothing and gets no op.
 */
export function cellRunOps(run: CellRun, pitch: number, m: CellMetrics): DrawOp[] {
  const ops: DrawOp[] = [];
  const y = run.row * m.boxSize;
  for (let k = 0; k < run.words.length; k++) {
    const x = (run.col + k) * pitch;
    const h = m.boxSize;
    const s = styledCell(run.words[k], run.row);
    ops.push({ kind: "fill", x, y, w: pitch, h, color: s.background });
    if (s.char !== NBSP) {
      ops.push({
        kind: "text", x, y, w: pitch, h, baseline: m.baseline, text: s.char, color: s.color,
      });
    }
    ops.push({ kind: "stroke", x, y, w: pitch, h, color: s.border });
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
    x: clamp(Math.floor((offsetX * canvasWidth) / boxWidth / m.halfSize),
             canvasWidth / m.halfSize),
    y: clamp(Math.floor((offsetY * canvasHeight) / boxHeight / m.boxSize),
             canvasHeight / m.boxSize),
  };
}
