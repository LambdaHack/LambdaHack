// Pure arithmetic for the fullscreen toggle: how to scale the fixed-size
// cell grid to fill the viewport with its aspect ratio kept, the browser
// analogue of SDL2's rendererLogicalSize letterboxing (Sdl.hs, the
// fullscreen branch of startup). No DOM; the wiring is fullscreen.ts.

export interface FitTransform {
  /** Uniform scale factor that fits the grid into the viewport. */
  scale: number;
  /** Horizontal offset in CSS px that centres the scaled grid. */
  dx: number;
  /** Vertical offset in CSS px that centres the scaled grid. */
  dy: number;
}

/**
 * Fit a gridW x gridH box, laid out with its left edge at gridLeft and
 * its top at 0, into a viewW x viewH viewport, scaled about its top left
 * corner. The offset is not assumed from the grid's auto margins, which
 * centre it only while it is narrower than the viewport; all three stay
 * finite for any positive grid.
 */
export function fitTransform(
  gridW: number,
  gridH: number,
  viewW: number,
  viewH: number,
  gridLeft: number,
): FitTransform {
  const scale = Math.min(viewW / gridW, viewH / gridH);
  return {
    scale,
    dx: (viewW - gridW * scale) / 2 - gridLeft,
    dy: (viewH - gridH * scale) / 2,
  };
}

/** The CSS transform that applies a fit, translate first so it is in px. */
export function fitTransformCss(t: FitTransform): string {
  return `translate(${t.dx}px, ${t.dy}px) scale(${t.scale})`;
}

/** The toggle button's label for the current state. */
export function toggleLabel(active: boolean): string {
  return active ? "Exit fullscreen" : "Fullscreen";
}
