// Drives the LambdaHack wasm *reactor* -- exe:LambdaHack as `make
// build-wasm` builds it, the binary the page loads -- under Node, with the
// page globals stubbed as run-wasm-test.mjs stubs them for the test suite,
// and checks the exports the page calls, which the test suite cannot
// exercise: its binary is a command, whose exports die with its RTS once
// `_start` returns, and it does not link Main.hs's lhStart. Item 3.2 of
// docs/wasm-frontend-work-list.md grows this into the benchmark driver.
//
// The checks lean on the game's default start, which plays itself
// ("press any key to regain control") and so submits frames steadily
// until a key arrives: the four exports exist; lhStart starts the game
// and its frames arrive, after one startup payload through lhConfigure
// carrying an allFontsScale, the X of an "--allFontsScale X" pair of args
// where there is one; a '?' through lhKey, a sync export taking the event,
// is taken from the browser before lhKey returns and stops the stream,
// which then stays quiet for a second, as it did not for 15s without the
// key; on that quiet screen, the main menu, a wheel south through lhWheel
// repaints it and a wheel north repaints it back exactly; lhMouseUp's
// column is in half-cells, as lhWheel's is: a left button release one
// half-cell past a menu choice's label repaints the menu unchanged, and
// one on the label's last half-cell, an odd column, activates the choice;
// C-P through lhKey saves one screenshot through lhPrintScreen, under a
// name of screenshotName's shape with png for its extension, and under
// --printEachScreen every frame that differs from the one before saves one
// besides; Ctrl+Shift with the numpad's '/' through lhKey paints what C-?
// does, as natively, and not what C-/ does; and lhKey leaves the browser
// a bare Control chord of '+', an Alt, AltGraph and Meta chord, and Shift
// pressed alone.
//
// Usage: node run-wasm-game.mjs <LambdaHack.wasm> <ghc_wasm_jsffi.mjs> [arg...]
//   <ghc_wasm_jsffi.mjs> is post-link.mjs's output for that binary, as in
//   run-wasm-test.mjs's usage, and the args go to lhStart as its WASI
//   argv. Exit 0 when every check holds, 1 when one fails, 2 when the run
//   did not happen.
//
// Non-vacuity: some of the checks' controls are mutants in
// tools/mutants-deep.py, each judged by `make test-reactor` and replayed by
// `selftest-mutants.py tools/mutants-deep.py`.

import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { basename, join } from "node:path";
import { fileURLToPath } from "node:url";
import { WASI } from "node:wasi";

import {
  installStubs,
  instantiate,
  requireFiles,
  sleep,
  waitFor as poll,
} from "./run-wasm-test.mjs";

const [, , wasmPath, jsffiPath, ...extraArgs] = process.argv;
const usage =
  "usage: node run-wasm-game.mjs <LambdaHack.wasm>" +
  " <ghc_wasm_jsffi.mjs> [arg...]";
requireFiles(usage, wasmPath, jsffiPath);

// The page's decoder of a frame's overlays, overlay-core.ts's, bundled from its
// source by esbuild, which npm ci in ts-src installs, so that the words are
// read as the page reads them.
async function loadDecoder() {
  try {
    const { build } = await import("esbuild");
    const source = new URL("src/overlay-core.ts", import.meta.url);
    const { outputFiles } = await build({
      entryPoints: [fileURLToPath(source)],
      bundle: true,
      format: "esm",
      write: false,
      logLevel: "silent",
    });
    const code = Buffer.from(outputFiles[0].contents).toString("base64");
    return (await import(`data:text/javascript;base64,${code}`))
      .decodeFrameLayout;
  } catch (e) {
    console.error(
      `${basename(process.argv[1])}: cannot bundle overlay-core.ts: ${e.message}`,
    );
    process.exit(2);
  }
}
const decodeFrameLayout = await loadDecoder();
const printEachScreen = extraArgs.includes("--printEachScreen");
// The scale an "--allFontsScale X" pair in the args gives, if any.
const scaleAt = extraArgs.indexOf("--allFontsScale");
const argScale = scaleAt < 0 ? undefined : Number(extraArgs[scaleAt + 1]);

const ok = (msg) => console.log(`ok   ${msg}`);
class Failed extends Error {}
const fail = (msg) => {
  throw new Failed(msg);
};

// The page's loader gives the reactor the argv ["LambdaHack"], no
// environment and an empty root directory; so does this, the root being
// a fresh temporary directory, Node's WASI preopening host directories,
// removed at exit, a failing one included, and the argv carrying this
// script's args after the program name.
const root = mkdtempSync(join(tmpdir(), "lh-reactor-"));
process.on("exit", () => rmSync(root, { recursive: true, force: true }));
const wasi = new WASI({
  version: "preview1",
  args: ["LambdaHack", ...extraArgs],
  env: {},
  preopens: { "/": root },
  returnOnExit: true,
});
let instance;
await installStubs(() => instance.exports.memory);
instance = await instantiate(wasmPath, jsffiPath, wasi);
const lh = instance.exports;
const calls = globalThis.lhSubmitFrameCalls;
const prints = globalThis.lhPrintScreenCalls;
const overlays = globalThis.lhSubmitOverlaysCalls;
// The overlays of frame i, which Wasm.hs submits just before the frame where it
// has any, if it has any.
const overlaysOf = (i) => overlays.find((o) => o.frames === i);
// The hash of frame i: its cells' hash continued over the words of its
// overlays, so the cells' hash alone where it has none.
function frameHash(i) {
  const words = overlaysOf(i)?.words ?? [];
  return words.reduce(
    (hash, word) => Math.imul(hash ^ word, 0x01000193) >>> 0,
    calls[i].hash,
  );
}
const lastHash = () => frameHash(calls.length - 1);
// The latest frame's text, a string per row, its square overlay laid over its
// cells, for finding a menu choice, which a menu in the square font shows there
// under the multi-font setup.
function screenRows() {
  const { w, h, words } = globalThis.lhLastFrame;
  const chars = Array.from(words, (word) => String.fromCodePoint(word >>> 16));
  const o = overlaysOf(calls.length - 1);
  const square = o ? decodeFrameLayout(o.words, o.w, w).square : [];
  for (const run of square) {
    run.words.forEach((word, k) => {
      chars[run.row * w + run.col + k] = String.fromCodePoint(word >>> 16);
    });
  }
  return Array.from({ length: h }, (_, y) =>
    chars.slice(y * w, (y + 1) * w).join(""),
  );
}
const noMods = [false, false, false, false];

// A stand-in for the page's KeyboardEvent: the fields lhKey reads, and
// a count of the calls that take the key from the browser.
function keyEvent(
  key,
  {
    ctrlKey = false,
    shiftKey = false,
    altKey = false,
    metaKey = false,
    altGraph = false,
  } = {},
) {
  return {
    key,
    ctrlKey,
    shiftKey,
    altKey,
    metaKey,
    prevented: 0,
    stopped: 0,
    getModifierState(k) {
      return k === "AltGraph" && altGraph;
    },
    preventDefault() {
      this.prevented += 1;
    },
    stopPropagation() {
      this.stopped += 1;
    },
  };
}

// lhStart's promise settles only when the game ends, which no check here
// asks for, so a settled one is a failure wherever it is noticed.
let ended = null;
function alive() {
  if (ended) fail(ended);
}

// Wait until `cond` holds, polling every 50ms, for at most `ms`.
function waitFor(cond, ms) {
  return poll(() => {
    alive();
    return cond();
  }, ms);
}

// Wait for `quietMs` with no new frame, for at most `ms`.
async function waitQuiet(quietMs, ms) {
  const end = Date.now() + ms;
  let n = calls.length;
  let since = Date.now();
  while (Date.now() < end) {
    await sleep(50);
    alive();
    if (calls.length !== n) {
      n = calls.length;
      since = Date.now();
    } else if (Date.now() - since >= quietMs) {
      return true;
    }
  }
  return false;
}

// Call an export with `args` on the quiet screen and wait for the
// repaint it causes; return that frame's hash.
async function repaint(name, args) {
  const n = calls.length;
  await lh[name](...args);
  if (!(await waitFor(() => calls.length > n, 5000))) {
    fail(`${name}(${args.slice(0, 3).join(", ")}) repainted nothing within 5s`);
  }
  await waitQuiet(500, 5000);
  return lastHash();
}

async function run() {
  const names = ["lhStart", "lhKey", "lhWheel", "lhMouseUp"];
  const missing = names.filter((n) => typeof lh[n] !== "function");
  if (missing.length) fail(`missing exports: ${missing.join(", ")}`);
  ok(`exports present: ${names.join(", ")}`);
  wasi.initialize(instance);

  lh.lhStart().then(
    () => {
      ended = "lhStart returned";
    },
    (e) => {
      ended = `lhStart threw: ${e}`;
    },
  );
  if (!(await waitFor(() => calls.length >= 10, 60000))) {
    fail(`only ${calls.length} frames within 60s of lhStart`);
  }
  const { w, h } = calls[0];
  ok(`lhStart starts the game: ${calls.length} frames of ${w}x${h} so far`);

  // Wasm.hs's startup hands the page one payload, before any frame.
  const configures = globalThis.lhConfigureCalls;
  const scale = configures[0]?.payload.allFontsScale;
  if (
    configures.length !== 1 ||
    configures[0].frames !== 0 ||
    typeof scale !== "number"
  ) {
    fail(`lhConfigure got ${JSON.stringify(configures)}`);
  }
  if (argScale !== undefined && scale !== argScale) {
    fail(`allFontsScale ${scale} in the payload, ${argScale} in the args`);
  }
  ok(
    "lhConfigure gets the startup payload before the first frame:" +
      ` allFontsScale ${scale}`,
  );

  const help = keyEvent("?", { shiftKey: true });
  if (lh.lhKey(help) instanceof Promise) fail("lhKey is not a sync export");
  if (help.prevented !== 1 || help.stopped !== 1) {
    fail(
      `lhKey left '?' to the browser: preventDefault ${help.prevented}` +
        ` times, stopPropagation ${help.stopped} times`,
    );
  }
  ok("lhKey takes '?' from the browser before it returns");
  if (!(await waitQuiet(1000, 15000))) {
    fail("frames kept coming for 15s after a '?' through lhKey");
  }
  ok(`lhKey reaches the game: the stream stopped at ${calls.length} frames`);

  const menu = lastHash();
  const scrolled = await repaint("lhWheel", [21, 10, 100, ...noMods]);
  const back = await repaint("lhWheel", [21, 10, -100, ...noMods]);
  if (scrolled === menu || back !== menu) {
    fail(`lhWheel south then north went ${menu} -> ${scrolled} -> ${back}`);
  }
  ok(
    "lhWheel reaches the game: south then north repaints" +
      ` ${menu} -> ${scrolled} -> ${back}`,
  );

  // A choice is clickable on its label's half-cells (SlideshowM.hs's
  // onChoice), so its last one is odd, and were the column a cell
  // doubled, as before 0.4, that click would miss as the one past it does.
  const label = "see command help";
  const rows = screenRows();
  const row = rows.findIndex((r) => r.includes(label));
  if (row < 0) fail(`no '${label}' on the menu:\n${rows.join("\n")}`);
  const last = 2 * (rows[row].indexOf(label) + label.length) - 1;
  const past = await repaint("lhMouseUp", [last + 1, row, 0, ...noMods]);
  if (past !== menu) {
    fail(
      `a left button release at (${last + 1}, ${row}), past '${label}',` +
        " changed the menu",
    );
  }
  const chosen = await repaint("lhMouseUp", [last, row, 0, ...noMods]);
  if (chosen === menu)
    fail(
      `a left button release at (${last}, ${row}), on '${label}', missed it`,
    );
  ok(
    `lhMouseUp reaches the game in half-cells: '${label}' ends at` +
      ` (${last}, ${row}), a release past it repaints the menu and one on it` +
      " activates the choice",
  );

  // On that quiet screen, with no timer due, a key through the sync lhKey
  // must be acted on with no other event to follow it.
  await repaint("lhKey", [keyEvent("Escape")]);
  ok("lhKey repaints a quiet screen: Escape is acted on with no other event");

  // Under --printEachScreen each frame that differs from the one before is
  // printed right after it is submitted, the first included, which Wasm.hs
  // compares with a blank frame, and otherwise none is; then the game's C-P,
  // Control and Shift with 'P', prints once more. The release past the menu
  // choice above repainted the menu unchanged, so a print of every frame fails
  // this. A frame's hash covers its cells and its overlays as laid out, which
  // is all of what Wasm.hs compares but overlay text cut at the screen's edge.
  const changed = () =>
    calls.flatMap((c, i) => {
      const before = calls[i - 1];
      const same =
        before &&
        c.w === before.w &&
        c.h === before.h &&
        frameHash(i) === frameHash(i - 1);
      return same ? [] : [i + 1];
    });
  const framePrints = () => (printEachScreen ? changed() : []);
  const expected = framePrints();
  if (
    prints.length !== expected.length ||
    !prints.every((p, i) => p.frames === expected[i])
  ) {
    fail(
      `${prints.length} screenshots for ${calls.length} frames,` +
        ` ${changed().length} of them changed, before C-P` +
        `${printEachScreen ? " under" : " without"} --printEachScreen`,
    );
  }
  const printed = prints.length;
  lh.lhKey(keyEvent("P", { ctrlKey: true, shiftKey: true }));
  if (!(await waitFor(() => prints.length > printed, 5000))) {
    fail("C-P through lhKey printed no screenshot within 5s");
  }
  await waitQuiet(500, 5000);
  if (prints.length !== framePrints().length + 1) {
    fail(
      `${prints.length} screenshots for ${calls.length} frames,` +
        ` ${changed().length} of them changed, after C-P`,
    );
  }
  const shape = /^prtscn\d{4}-\d\d-\d\d_\d\d\.\d\d\.\d\d(\.\d+)?\.png$/;
  const bad = prints.find((p) => !shape.test(p.name));
  if (bad) fail(`lhPrintScreen got the name ${bad.name}`);
  const which = printEachScreen ? "each changed frame" : "no frame";
  ok(
    `C-P prints one screenshot and ${which} one: ${prints.length} for` +
      ` ${calls.length} frames, ${changed().length} of them changed,` +
      ` the last ${prints.at(-1).name}`,
  );

  // Ctrl+Shift with the numpad's Divide, whose key reads '/' under Shift,
  // is C-? natively, Sdl.hs translating it under the raw Shift bit, and so
  // here, lhKey handing keyTranslateWeb that bit: after C-/, which sets
  // the crosshair on the nearest item or says there is none, it paints
  // what C-? does after C-/, typed as '?'. The second C-/'s frame is not
  // compared with the first's: where neither finds an item, C-? has moved
  // the crosshair between them.
  const ctrlShift = { ctrlKey: true, shiftKey: true };
  const item = await repaint("lhKey", [keyEvent("/", { ctrlKey: true })]);
  const unknown = await repaint("lhKey", [keyEvent("?", ctrlShift)]);
  if (unknown === item) fail(`C-? and C-/ both painted ${item}`);
  await repaint("lhKey", [keyEvent("/", { ctrlKey: true })]);
  const divide = await repaint("lhKey", [keyEvent("/", ctrlShift)]);
  if (divide !== unknown) {
    fail(
      `Ctrl+Shift with the numpad's '/' painted ${divide}, not C-?'s` +
        ` ${unknown}${divide === item ? ", but C-/'s" : ""}`,
    );
  }
  ok(
    "Ctrl+Shift with the numpad's '/' is C-?, as natively: after C-/" +
      ` it paints ${divide}, as C-? does, where C-/ paints ${item}`,
  );

  // Last, since these reach the game too, whose screen nothing checks after.
  const passed = [
    ["Control '+'", keyEvent("+", { ctrlKey: true })],
    ["Alt 'x'", keyEvent("x", { altKey: true })],
    ["AltGraph 'x'", keyEvent("x", { altGraph: true })],
    ["Meta 'x'", keyEvent("x", { metaKey: true })],
    ["Shift alone", keyEvent("Shift", { shiftKey: true })],
  ];
  for (const [what, e] of passed) {
    lh.lhKey(e);
    if (e.prevented || e.stopped) fail(`lhKey took ${what} from the browser`);
  }
  ok(`lhKey leaves the browser ${passed.map(([what]) => what).join(", ")}`);
  alive();
}

try {
  await run();
} catch (e) {
  console.error(`FAIL: ${e instanceof Failed ? e.message : (e?.stack ?? e)}`);
  process.exitCode = 1;
}
process.exit(process.exitCode ?? 0);
