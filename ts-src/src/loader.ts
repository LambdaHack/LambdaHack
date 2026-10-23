// Browser loader wiring: instantiate the LambdaHack wasm reactor with an
// in-memory WASI shim and the generated JSFFI glue, hook up frame submission,
// screenshots, the startup payload + keyboard, then start the game and say
// on the page how it ended. Mirrors ../run-wasm-game.mjs (the Node driver
// for the reactor), but paints a canvas instead of recording frames. Not
// unit-tested.

import {
  WASI,
  OpenFile,
  File,
  ConsoleStdout,
  PreopenDirectory,
} from "@bjorn3/browser_wasi_shim";
import {
  mountTerminal,
  type MouseHandler,
  type OverlayFonts,
  type WheelHandler,
} from "./terminal.js";
import { leaveFullscreen, mountFullscreen } from "./fullscreen.js";

interface LhExports {
  memory: WebAssembly.Memory;
  lhStart: () => Promise<void>;
  // A sync export, so that it decides the key's default action before
  // the keydown dispatch returns.
  lhKey: (e: KeyboardEvent) => void;
  // Taking what terminal.ts's handlers pass.
  lhWheel: (...a: Parameters<WheelHandler>) => Promise<void>;
  lhMouseUp: (...a: Parameters<MouseHandler>) => Promise<void>;
}

// The startup payload Wasm.hs's startup hands lhConfigure: named fields, each
// present when the engine has its value, any others ignored.
interface StartupPayload {
  // The scale Sdl.hs multiplies its font sizes by.
  allFontsScale?: number;
  // The chosen fontset's fonts but the map font, each a file deployed beside
  // the page and a size in CSS pixels, that scale applied.
  fonts?: OverlayFonts;
}

declare global {
  // eslint-disable-next-line no-var
  var lhSubmitFrame: ((addr: number, w: number, h: number) => void) | undefined;
  // eslint-disable-next-line no-var
  var lhPrintScreen: ((name: string) => void) | undefined;
  // eslint-disable-next-line no-var
  var lhConfigure: ((json: string) => void) | undefined;
}

async function main(): Promise<void> {
  const screen = document.getElementById("screen");
  if (!screen) throw new Error("missing #screen element");
  const status = document.getElementById("status");

  // In-memory filesystem: stdin, console stdout/stderr, and a writable root
  // for the game's data dir (/LambdaHack). Contents here are NOT retained
  // across loads, but that's fine: save-file persistence is handled
  // separately by WasmFile.hs, which talks to localStorage directly via
  // JSFFI and never touches this filesystem at all. Only genuinely
  // throwaway/scratch I/O should end up going through this.
  const fds = [
    new OpenFile(new File([])),
    ConsoleStdout.lineBuffered((line) => console.log("[lh]", line)),
    ConsoleStdout.lineBuffered((line) => console.warn("[lh]", line)),
    new PreopenDirectory("/", new Map()),
  ];
  const wasi = new WASI(["LambdaHack"], [], fds);

  // The module is compiled as it downloads, and the glue fetched meanwhile;
  // a static server that does not serve .wasm as application/wasm fails
  // the streaming compile, and then the bytes are compiled whole.
  const url = "./LambdaHack.wasm";
  const [mod, jsffi] = await Promise.all([
    WebAssembly.compileStreaming(fetch(url)).catch(async () =>
      WebAssembly.compile(await (await fetch(url)).arrayBuffer()),
    ),
    // @ts-expect-error generated at build time by post-link.mjs, served alongside.
    import("./ghc_wasm_jsffi.mjs"),
  ]);
  const jsffiFactory = jsffi.default;
  const importExports: Record<string, unknown> = {};
  const inst = await WebAssembly.instantiate(mod, {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: jsffiFactory(importExports),
  });
  Object.assign(importExports, inst.exports);

  const lh = inst.exports as unknown as LhExports;
  const term = mountTerminal(
    screen,
    () => lh.memory,
    (e) => lh.lhKey(e),
    (...a) => void lh.lhWheel(...a),
    (...a) => void lh.lhMouseUp(...a),
  );
  globalThis.lhSubmitFrame = (addr, w, h) => term.submitFrame(addr, w, h);
  globalThis.lhPrintScreen = (name) => term.printScreen(name);
  globalThis.lhConfigure = (json) => {
    const payload = JSON.parse(json) as StartupPayload;
    if (payload.allFontsScale !== undefined)
      term.setFontScale(payload.allFontsScale);
    if (payload.fonts !== undefined) term.setFonts(payload.fonts);
  };
  const banner = document.getElementById("banner");
  mountFullscreen(
    document.getElementById("fsToggle"),
    screen,
    banner ? [banner] : [],
  );

  wasi.initialize(
    inst as unknown as {
      exports: { memory: WebAssembly.Memory; _initialize?: () => unknown };
    },
  );
  // Mirrors Dom.hs's replaceChild_ of the "pleaseWait" placeholder: drop the
  // loading message only once everything is wired and about to start, not
  // before, so it stays visible for the whole duration of a slow load.
  status?.remove();
  // lhStart's promise settles when the game ends, and under the success rule
  // measured in docs/wasm-frontend-unified-plan.md's 3.2 a resolution or
  // a rejection with the message ExitSuccess is a finished game and any
  // other rejection a crash. The page says which, since its screen goes on
  // showing the last frame.
  lh.lhStart().then(
    () => showEnd(FINISHED),
    (e: unknown) => {
      if (e instanceof Error && e.message === "ExitSuccess") {
        showEnd(FINISHED);
      } else {
        console.error(e);
        const message = e instanceof Error ? e.message : String(e);
        showEnd(
          `The game crashed: ${message}. See console for details.` +
            " Reload the page to play again.",
          RED,
        );
      }
    },
  );
}

const FINISHED = "The game has ended. Reload the page to play again.";
const RED = "#d50505"; // From the game's own palette

// Says how the game ended in place of the banner's text, leaving fullscreen
// first, since fullscreen hides the banner.
function showEnd(text: string, color?: string): void {
  leaveFullscreen();
  const bannerText = document.getElementById("bannerText");
  if (!bannerText) return;
  bannerText.textContent = text;
  if (color) bannerText.style.color = color;
}

main().catch((e) => {
  console.error(e);
  const status = document.getElementById("status");
  if (status) {
    status.textContent =
      "Failed to load LambdaHack wasm. See console for details.";
    status.style.color = RED;
  }
});
