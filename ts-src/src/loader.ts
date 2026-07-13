// Browser loader wiring: instantiate the LambdaHack wasm reactor with an
// in-memory WASI shim and the generated JSFFI glue, hook up frame submission
// + keyboard, then start the game. Mirrors web/harness.mjs (the node
// integration harness), but renders to the DOM instead of capturing frames.
// Not unit-tested.

import { WASI, OpenFile, File, ConsoleStdout, PreopenDirectory } from "@bjorn3/browser_wasi_shim";
import { mountTerminal } from "./terminal.js";

interface LhExports {
  memory: WebAssembly.Memory;
  lhStart: () => Promise<void>;
  lhKey: (key: string, ctrl: boolean, shift: boolean, alt: boolean, meta: boolean) => Promise<void>;
  lhWheel: (
    col: number,
    row: number,
    deltaY: number,
    ctrl: boolean,
    shift: boolean,
    alt: boolean,
    meta: boolean,
  ) => Promise<void>;
  lhMouseUp: (
    col: number,
    row: number,
    button: number,
    ctrl: boolean,
    shift: boolean,
    alt: boolean,
    meta: boolean,
  ) => Promise<void>;
}

declare global {
  // eslint-disable-next-line no-var
  var lhSubmitFrame: ((addr: number, w: number, h: number) => void) | undefined;
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

  // Plain compile (not compileStreaming) so it works on static servers that
  // don't serve .wasm with the application/wasm MIME type.
  const wasmBytes = await (await fetch("./LambdaHack.wasm")).arrayBuffer();
  const mod = await WebAssembly.compile(wasmBytes);
  // @ts-expect-error generated at build time by post-link.mjs, served alongside.
  const jsffiFactory = (await import("./ghc_wasm_jsffi.mjs")).default;
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
    (k, c, s, a, m) => {
      void lh.lhKey(k, c, s, a, m);
    },
    (col, row, deltaY, c, s, a, m) => {
      void lh.lhWheel(col, row, deltaY, c, s, a, m);
    },
    (col, row, button, c, s, a, m) => {
      void lh.lhMouseUp(col, row, button, c, s, a, m);
    },
  );
  globalThis.lhSubmitFrame = (addr, w, h) => term.submitFrame(addr, w, h);

  wasi.initialize(inst as unknown as { exports: { memory: WebAssembly.Memory; _initialize?: () => unknown } });
  // Mirrors Dom.hs's replaceChild_ of the "pleaseWait" placeholder: drop the
  // loading message only once everything is wired and about to start, not
  // before, so it stays visible for the whole duration of a slow load.
  status?.remove();
  void lh.lhStart();
}

main().catch((e) => {
  console.error(e);
  const status = document.getElementById("status");
  if (status) {
    status.textContent = "Failed to load LambdaHack wasm. See console for details.";
    status.style.color = "#d50505"; // Red, from the game's own palette
  }
});
