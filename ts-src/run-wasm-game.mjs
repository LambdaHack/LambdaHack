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
// and its frames arrive; a '?' through lhKey stops the stream, which then
// stays quiet for a second, as it did not for 15s without the key;
// on that quiet screen a wheel south through lhWheel repaints it and a
// wheel north repaints it back exactly; and a left button release through
// lhMouseUp repaints it too.
//
// Usage: node run-wasm-game.mjs <LambdaHack.wasm> <ghc_wasm_jsffi.mjs>
//   <ghc_wasm_jsffi.mjs> is post-link.mjs's output for that binary, as in
//   run-wasm-test.mjs's usage. Exit 0 when every check holds, 1 when one
//   fails, 2 when the run did not happen.

import { mkdtempSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { WASI } from 'node:wasi';

import { installStubs, instantiate, requireFiles } from './run-wasm-test.mjs';

const [, , wasmPath, jsffiPath] = process.argv;
requireFiles('usage: node run-wasm-game.mjs <LambdaHack.wasm> <ghc_wasm_jsffi.mjs>',
             wasmPath, jsffiPath);

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const ok = (msg) => console.log(`ok   ${msg}`);
class Failed extends Error {}
const fail = (msg) => { throw new Failed(msg); };

// The page's loader gives the reactor the argv ["LambdaHack"], no
// environment and an empty root directory; so does this, the root being
// a fresh temporary directory, Node's WASI preopening host directories,
// removed at exit, a failing one included.
const root = mkdtempSync(join(tmpdir(), 'lh-reactor-'));
process.on('exit', () => rmSync(root, { recursive: true, force: true }));
const wasi = new WASI({
  version: 'preview1',
  args: ['LambdaHack'],
  env: {},
  preopens: { '/': root },
  returnOnExit: true,
});
let instance;
await installStubs(() => instance.exports.memory);
instance = await instantiate(wasmPath, jsffiPath, wasi);
const lh = instance.exports;
const calls = globalThis.lhSubmitFrameCalls;
const noMods = [false, false, false, false];

// lhStart's promise settles only when the game ends, which no check here
// asks for, so a settled one is a failure wherever it is noticed.
let ended = null;
function alive() {
  if (ended) fail(ended);
}

// Wait until `cond` holds, polling every 50ms, for at most `ms`.
async function waitFor(cond, ms) {
  const end = Date.now() + ms;
  for (;;) {
    alive();
    if (cond()) return true;
    if (Date.now() >= end) return false;
    await sleep(50);
  }
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
    fail(`${name}(${args.slice(0, 3).join(', ')}) repainted nothing within 5s`);
  }
  await waitQuiet(500, 5000);
  return calls[calls.length - 1].hash;
}

async function run() {
  const names = ['lhStart', 'lhKey', 'lhWheel', 'lhMouseUp'];
  const missing = names.filter((n) => typeof lh[n] !== 'function');
  if (missing.length) fail(`missing exports: ${missing.join(', ')}`);
  ok(`exports present: ${names.join(', ')}`);
  wasi.initialize(instance);

  lh.lhStart().then(() => { ended = 'lhStart returned'; },
                    (e) => { ended = `lhStart threw: ${e}`; });
  if (!(await waitFor(() => calls.length >= 10, 60000))) {
    fail(`only ${calls.length} frames within 60s of lhStart`);
  }
  const { w, h } = calls[0];
  ok(`lhStart starts the game: ${calls.length} frames of ${w}x${h} so far`);

  await lh.lhKey('?', ...noMods);
  if (!(await waitQuiet(1000, 15000))) {
    fail("frames kept coming for 15s after a '?' through lhKey");
  }
  ok(`lhKey reaches the game: the stream stopped at ${calls.length} frames`);

  const menu = calls[calls.length - 1].hash;
  const scrolled = await repaint('lhWheel', [10, 10, 100, ...noMods]);
  const back = await repaint('lhWheel', [10, 10, -100, ...noMods]);
  if (scrolled === menu || back !== menu) {
    fail(`lhWheel south then north went ${menu} -> ${scrolled} -> ${back}`);
  }
  ok(`lhWheel reaches the game: south then north repaints ${menu} -> ${scrolled} -> ${back}`);

  await repaint('lhMouseUp', [10, 10, 0, ...noMods]);
  ok('lhMouseUp reaches the game: a left button release repaints the screen');
  alive();
}

try {
  await run();
} catch (e) {
  console.error(`FAIL: ${e instanceof Failed ? e.message : (e?.stack ?? e)}`);
  process.exitCode = 1;
}
process.exit(process.exitCode ?? 0);
