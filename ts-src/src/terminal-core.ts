// Pure rendering logic for the LambdaHack wasm web frontend: turn the engine's
// raw AttrCharW32 cell words into glyphs + colors. No DOM, no wasm — this is the
// only unit-tested part of the web UI (the loader and the actual drawing are
// thin wiring around these functions).
//
// Encoding (see LambdaHack's Definition/Color.hs attrCharToW32):
//   char codepoint = w >>> 16 ; fg = (w >>> 8) & 0xFF ; bg = w & 0xFF
// fg indexes the Color enum, bg indexes the Highlight enum.

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

// Color enum index -> hex (Definition/Color.hs colorToRGB), indexed by fromEnum.
const PALETTE: readonly string[] = [
  "#000000", // Black
  "#D50505", // Red
  "#059D05", // Green
  "#CA4A05", // Brown
  "#0556F4", // Blue
  "#AF0EAF", // Magenta
  "#059696", // Cyan
  "#B8BFCB", // White
  "#C4BEB1", // AltWhite
  "#6F5F5F", // BrBlack
  "#FF5555", // BrRed
  "#65F136", // BrGreen
  "#EBD642", // BrYellow
  "#4D98F4", // BrBlue
  "#FF77FF", // BrMagenta
  "#52F4E5", // BrCyan
  "#FFFFFF", // BrWhite
];

// Highlight enum index -> Color enum index (Definition/Color.hs highlightToColor).
const HIGHLIGHT_TO_COLOR: readonly number[] = [
  0, // HighlightNone -> Black
  9, // HighlightBackground -> BrBlack
  2, // HighlightGreen -> Green
  4, // HighlightBlue -> Blue
  3, // HighlightBrown -> Brown
  6, // HighlightCyan -> Cyan
  9, // HighlightGrey -> BrBlack
  7, // HighlightWhite -> White
  14, // HighlightMagenta -> BrMagenta
  1, // HighlightRed -> Red
  12, // HighlightYellow -> BrYellow
  12, // HighlightYellowAim -> BrYellow
  1, // HighlightRedAim -> Red
  0, // HighlightNoneCursor -> Black
];

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
  return bg === HIGHLIGHT_BACKGROUND ? "#251F1F" : "#000000";
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
    return PALETTE[0];
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
