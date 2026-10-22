// Fullscreen wiring: a button toggles the page in and out of the
// Fullscreen API, and while it is in, the keyboard is locked to the page
// and the game's canvas is scaled to fill the viewport with its aspect ratio
// kept (the arithmetic is fullscreen-core).
// Page-level only: sfullscreenMode is a startup-time SDL choice with no
// in-game command even natively. Not unit-tested (DOM wiring).

import {
  fitTransform,
  fitTransformCss,
  toggleLabel,
} from "./fullscreen-core.js";

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

  // Fullscreen with the keyboard locked hands the page the Control keys
  // the browser keeps in a window: Firefox (151 on) through the keyboardLock
  // option, which Chromium ignores, and Chromium through navigator.keyboard,
  // which Firefox lacks. Under either lock a held Escape still leaves.
  const keyboard = (
    navigator as Navigator & {
      keyboard?: { lock?: () => Promise<void>; unlock?: () => void };
    }
  ).keyboard;
  async function enter(): Promise<void> {
    const options = { keyboardLock: "browser" } as FullscreenOptions;
    await document.documentElement.requestFullscreen(options);
    await keyboard?.lock?.();
  }

  // offsetWidth/offsetHeight read the layout box, which a transform does
  // not change, so the fit is stable under its own application.
  function applyFit(): void {
    if (active()) {
      const t = fitTransform(
        screen.offsetWidth,
        screen.offsetHeight,
        window.innerWidth,
        window.innerHeight,
      );
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
    if (!on) keyboard?.unlock?.();
    button!.textContent = toggleLabel(on);
    button!.style.position = on ? "fixed" : "";
    button!.style.top = on ? "4px" : "";
    button!.style.right = on ? "8px" : "";
    applyFit();
  }

  button.addEventListener("click", () => {
    // requestFullscreen needs a trusted user gesture: this click is one.
    const p = active() ? document.exitFullscreen() : enter();
    p.catch((e: unknown) => console.warn("[lh] fullscreen toggle failed", e));
    // The click moved focus to the button; keys are read on the screen.
    screen.focus();
  });
  // F11 leaves fullscreen, as it does without the lock: Firefox's lock keeps
  // F11 for Firefox, which leaves by itself, but Chromium's hands it to the
  // page and then ignores it, prevented or not. Caught before the game's
  // listener, so the game never sees it.
  document.addEventListener(
    "keydown",
    (e) => {
      if (e.key !== "F11" || !active()) return;
      e.preventDefault();
      e.stopPropagation();
      document
        .exitFullscreen()
        .catch((err: unknown) =>
          console.warn("[lh] fullscreen exit failed", err),
        );
    },
    true,
  );
  // Covers the browser's own exit (Escape, F11) as well as our toggle.
  document.addEventListener("fullscreenchange", sync);
  window.addEventListener("resize", applyFit);
  sync();
}
