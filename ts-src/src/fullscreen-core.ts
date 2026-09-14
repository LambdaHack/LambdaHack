// Pure arithmetic for the fullscreen toggle: how to scale the fixed-size
// cell grid to fill the viewport with its aspect ratio kept, the browser
// analogue of SDL2's rendererLogicalSize letterboxing (Sdl.hs, the
// fullscreen branch of startup). No DOM; the wiring is fullscreen.ts.

export interface FitTransform {
  /** Uniform scale factor that fits the grid into the viewport. */
  scale: number;
  /** Vertical offset in CSS px that centres the scaled grid. */
  dy: number;
}

/**
 * Fit a gridW x gridH box into a viewW x viewH viewport. The grid is
 * already centred horizontally by its own margin, so only the vertical
 * offset is returned; both stay finite for any positive grid.
 */
export function fitTransform(
  gridW: number,
  gridH: number,
  viewW: number,
  viewH: number,
): FitTransform {
  const scale = Math.min(viewW / gridW, viewH / gridH);
  return { scale, dy: (viewH - gridH * scale) / 2 };
}

/** The CSS transform that applies a fit, translate first so it is in px. */
export function fitTransformCss(t: FitTransform): string {
  return `translateY(${t.dy}px) scale(${t.scale})`;
}

/** The toggle button's label for the current state. */
export function toggleLabel(active: boolean): string {
  return active ? "Exit fullscreen" : "Fullscreen";
}
