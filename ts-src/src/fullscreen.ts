// Fullscreen wiring: a button toggles the page in and out of the
// Fullscreen API, and while it is in, the game's canvas is scaled to fill the
// viewport with its aspect ratio kept (the arithmetic is fullscreen-core).
// Page-level only: sfullscreenMode is a startup-time SDL choice with no
// in-game command even natively. Not unit-tested (DOM wiring).

import { fitTransform, fitTransformCss, toggleLabel } from "./fullscreen-core.js";

export function mountFullscreen(
  button: HTMLElement | null,
  screen: HTMLElement,
  chrome: HTMLElement[],
): void {
  if (!button) return;
  if (!document.fullscreenEnabled) {
    // An embedding that forbids it, e.g. an iframe without allowfullscreen.
    button.hidden = true;
    return;
  }

  function active(): boolean {
    return document.fullscreenElement !== null;
  }

  // offsetWidth/offsetHeight read the layout box, which a transform does
  // not change, so the fit is stable under its own application.
  function applyFit(): void {
    if (active()) {
      const t = fitTransform(screen.offsetWidth, screen.offsetHeight, window.innerWidth, window.innerHeight);
      screen.style.transformOrigin = "top center";
      screen.style.transform = fitTransformCss(t);
    } else {
      screen.style.transform = "";
    }
  }

  function sync(): void {
    const on = active();
    // The banner is page chrome; in fullscreen only the game is shown,
    // as SDL2 shows only its window. The button is not inside it -- a
    // hidden ancestor would hide it too, and Escape would be the only way
    // out -- and floats over the letterbox while fullscreen is on.
    for (const el of chrome) el.hidden = on;
    button!.textContent = toggleLabel(on);
    button!.style.position = on ? "fixed" : "";
    button!.style.top = on ? "4px" : "";
    button!.style.right = on ? "8px" : "";
    applyFit();
  }

  button.addEventListener("click", () => {
    // requestFullscreen needs a trusted user gesture: this click is one.
    const p = active() ? document.exitFullscreen() : document.documentElement.requestFullscreen();
    p.catch((e: unknown) => console.warn("[lh] fullscreen toggle failed", e));
    // The click moved focus to the button; keys are read on the screen.
    screen.focus();
  });
  // Covers the browser's own exit (Escape, F11) as well as our toggle.
  document.addEventListener("fullscreenchange", sync);
  window.addEventListener("resize", applyFit);
  sync();
}
