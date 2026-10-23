// Pure arithmetic for the fullscreen toggle: the scale that fits the
// fixed-size cell grid to the viewport with its aspect ratio kept, the
// browser analogue of SDL2's rendererLogicalSize letterboxing (Sdl.hs, the
// fullscreen branch of startup); the page's CSS centres the scaled grid.
// No DOM; the wiring is fullscreen.ts.

/**
 * The uniform scale that fits a gridW x gridH box into a viewW x viewH
 * viewport, or 1 for a box not yet sized, as before the first frame sizes
 * the canvas.
 */
export function fitScale(
  gridW: number,
  gridH: number,
  viewW: number,
  viewH: number,
): number {
  if (gridW <= 0 || gridH <= 0) return 1;
  return Math.min(viewW / gridW, viewH / gridH);
}

/** The toggle button's label for the current state. */
export function toggleLabel(active: boolean): string {
  return active ? "Exit fullscreen" : "Fullscreen";
}
