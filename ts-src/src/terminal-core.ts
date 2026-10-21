// Pure rendering logic for the LambdaHack wasm web frontend: turn the engine's
// raw AttrCharW32 cell words into glyphs + colors. No DOM, no wasm — this is the
// only unit-tested part of the web UI (the loader and the actual drawing are
// thin wiring around these functions).
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
