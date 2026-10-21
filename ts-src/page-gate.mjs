// The deployed-page gate: build outputs in a directory, served here
// and loaded in headless Firefox over WebDriver BiDi, must boot the
// reactor, paint, and repaint on a key, on a quiet screen too. It is the
// one check in the repository that asks whether the page players load
// works, and it runs locally, before every browser landing, by ruling
// (docs/wasm-frontend-work-list.md, the page gate under Handing an item);
// CI stays blind to the page by that same ruling. Exit 0 when every
// assertion holds, 1 when one fails, 2 when the run did not happen: no
// Firefox, no BiDi endpoint, no server.
//
// Usage: node page-gate.mjs <site-dir>   (Node 22+, for the global WebSocket)
//   PAGE_GATE_OVERLAYS=1 also requires an overlay submission, once 2.4 lands.
//
// What is observed, and how: a preload script wraps the globalThis hooks
// the loader installs -- lhSubmitFrame, and lhSubmitOverlays once it exists
// -- by defining them as accessor properties before loader.ts assigns them,
// so every call is counted with its size and the page's own code runs
// unchanged. The screen is read back as a hash of #screen's innerHTML, or
// of the canvas's data URL once 0.4 replaces the grid, so a key's effect
// is a changed hash rather than a claim about what it drew.
//
// Non-vacuity, run 2026-09-14: a site directory without LambdaHack.wasm
// fails both frame assertions with exit 1, and PAGE_GATE_OVERLAYS=1 fails
// against a tree before 2.4; the same tree passes with six frames of 80x24
// and a '?' repainting.

import { spawn } from "node:child_process";
import { mkdir, writeFile } from "node:fs/promises";
import { join, resolve } from "node:path";

const site = process.argv[2];
if (!site) { console.error("usage: node page-gate.mjs <site-dir>"); process.exit(2); }
const wantOverlays = process.env.PAGE_GATE_OVERLAYS === "1";
const PORT = 8123;
const BROWSER = process.env.BROWSER ?? "firefox-beta";

function blocked(msg) { console.error("BLOCKED: " + msg); process.exit(2); }
function fail(msg) { console.error("FAIL: " + msg); process.exitCode = 1; }
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// --- static server, the bundle's own serve.mjs -------------------------
const server = spawn("node", [join(site, "serve.mjs"), site, String(PORT)], { stdio: ["ignore", "pipe", "inherit"] });
await new Promise((ok, no) => {
  server.stdout.on("data", (d) => { if (String(d).includes("serving")) ok(); });
  server.on("exit", (c) => no(new Error(`serve.mjs exited ${c}`)));
}).catch((e) => blocked(e.message));

// --- headless Firefox with a scratch profile ---------------------------
const profile = resolve(site, "ff-profile");
await mkdir(profile, { recursive: true });
await writeFile(join(profile, "user.js"), [
  'user_pref("browser.shell.checkDefaultBrowser", false);',
  'user_pref("browser.startup.homepage_override.mstone", "ignore");',
  'user_pref("browser.aboutwelcome.enabled", false);',
  'user_pref("datareporting.policy.dataSubmissionPolicyBypassNotification", true);',
  'user_pref("toolkit.telemetry.reportingpolicy.firstRun", false);',
  "",
].join("\n"));
const ff = spawn(BROWSER, ["--headless", "--no-remote", "--profile", profile, "--remote-debugging-port=0", "about:blank"],
  { stdio: ["ignore", "pipe", "pipe"] });
ff.on("error", (e) => blocked(`cannot start ${BROWSER}: ${e.message}`));
const wsUrl = await new Promise((ok) => {
  const t = setTimeout(() => ok(null), 30000);
  const look = (d) => { const m = /WebDriver BiDi listening on (ws:\/\/\S+)/.exec(String(d)); if (m) { clearTimeout(t); ok(m[1]); } };
  ff.stdout.on("data", look); ff.stderr.on("data", look);
});
if (!wsUrl) blocked(`${BROWSER} printed no BiDi endpoint within 30s`);

// --- a minimal BiDi client -------------------------------------------
const ws = new WebSocket(wsUrl + "/session");
await new Promise((ok, no) => { ws.onopen = ok; ws.onerror = () => no(new Error("websocket")); }).catch((e) => blocked(e.message));
let nextId = 1; const pending = new Map();
ws.onmessage = (ev) => { const m = JSON.parse(ev.data); if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); } };
function send(method, params) {
  return new Promise((ok) => { const id = nextId++; pending.set(id, ok); ws.send(JSON.stringify({ id, method, params })); })
    .then((m) => { if (m.type === "error") throw new Error(`${method}: ${m.error} ${m.message}`); return m.result; });
}
await send("session.new", { capabilities: {} });
const tree = await send("browsingContext.getTree", {});
const context = tree.contexts[0].context;
await send("script.addPreloadScript", { functionDeclaration: `() => {
  const g = { frames: 0, sizes: [], overlays: 0 };
  globalThis.__lhGate = g;
  let frame; let overlay;
  Object.defineProperty(globalThis, "lhSubmitFrame", { configurable: true,
    get() { return frame; },
    set(f) { frame = (a, w, h) => { g.frames++; g.sizes.push([w, h]); return f(a, w, h); }; } });
  Object.defineProperty(globalThis, "lhSubmitOverlays", { configurable: true,
    get() { return overlay; },
    set(f) { overlay = (...xs) => { g.overlays++; return f(...xs); }; } });
}` });
async function evaluate(expression) {
  const r = await send("script.evaluate", { expression, target: { context }, awaitPromise: true, resultOwnership: "none" });
  if (r.type !== "success") throw new Error(`evaluate failed: ${JSON.stringify(r).slice(0, 300)}`);
  return r.result.value;
}
const HASH = `(() => { const el = document.getElementById("screen"); const c = el && el.querySelector("canvas");
  const s = c ? c.toDataURL() : (el ? el.innerHTML : ""); let h = 0;
  for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) | 0; return String(h) + ":" + s.length; })()`;
async function gate() {
  await send("browsingContext.navigate", { context, url: `http://localhost:${PORT}/`, wait: "complete" });
  // 1. frames arrive after lhStart.
  let frames = 0;
  for (let i = 0; i < 120 && frames < 2; i++) { await sleep(500); frames = await evaluate("globalThis.__lhGate.frames"); }
  const size = await evaluate("JSON.stringify(globalThis.__lhGate.sizes[0] ?? null)");
  if (frames < 2) fail(`no second frame within 60s of load (frames=${frames})`);
  else console.log(`ok   frames arrive: ${frames} within the wait, the first ${size} cells`);
  // 2. a key changes the frame: '?' opens the help overlay.
  const before = await evaluate(HASH);
  const framesBefore = await evaluate("globalThis.__lhGate.frames");
  await send("input.performActions", { context, actions: [{ type: "key", id: "kbd",
    actions: [{ type: "keyDown", value: "?" }, { type: "keyUp", value: "?" }] }] });
  let after = before, framesAfter = framesBefore;
  for (let i = 0; i < 40 && after === before; i++) { await sleep(250); after = await evaluate(HASH); framesAfter = await evaluate("globalThis.__lhGate.frames"); }
  if (after === before) fail(`the screen did not change within 10s of a '?' key (frames ${framesBefore} -> ${framesAfter})`);
  else console.log(`ok   a key changes the frame: screen ${before} -> ${after}, frames ${framesBefore} -> ${framesAfter}`);
  // 3. a key on a quiet screen changes it too: with the game idle no timer
  // re-enters the RTS scheduler, which a key delivered by a sync export once
  // waited for. Quiet is a second with no frame; Escape leaves the help.
  let seen = await evaluate("globalThis.__lhGate.frames"), still = 0;
  for (let i = 0; i < 40 && still < 4; i++) {
    await sleep(250);
    const f = await evaluate("globalThis.__lhGate.frames");
    still = f === seen ? still + 1 : 0; seen = f;
  }
  const quiet = await evaluate(HASH);
  await send("input.performActions", { context, actions: [{ type: "key", id: "kbd",
    actions: [{ type: "keyDown", value: "\uE00C" }, { type: "keyUp", value: "\uE00C" }] }] });
  let changed = quiet;
  for (let i = 0; i < 12 && changed === quiet; i++) { await sleep(250); changed = await evaluate(HASH); }
  if (still < 4) fail("the screen never went quiet for a second within 10s of the '?'");
  else if (changed === quiet) fail(`a quiet screen did not change within 3s of an Escape key (frames ${seen})`);
  else console.log(`ok   a key changes a quiet screen: ${quiet} -> ${changed}`);
  // 4. an overlay submission, once 2.4 lands.
  const overlays = await evaluate("globalThis.__lhGate.overlays");
  if (wantOverlays) {
    if (overlays < 1) fail("no overlay submission after the key (PAGE_GATE_OVERLAYS=1)");
    else console.log(`ok   overlays submitted: ${overlays}`);
  } else console.log(`note overlays submitted: ${overlays} (not required until 2.4; PAGE_GATE_OVERLAYS=1 requires one)`);
}
try { await gate(); } catch (e) { fail(String(e.message ?? e)); }
finally { ws.close(); ff.kill(); server.kill(); }
process.exit(process.exitCode ?? 0);
