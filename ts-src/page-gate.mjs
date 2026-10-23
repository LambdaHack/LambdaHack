// The deployed-page gate: build outputs in a directory, served here and loaded
// in headless Firefox and headless Chromium over WebDriver BiDi, Chromium's
// through chromedriver, must boot the reactor, paint, stop its self-play
// on a key and repaint on a key on a quiet screen, in each browser. It
// is the one check in the repository that asks whether the page players
// load works, and it runs locally, before every browser landing, by ruling
// (docs/wasm-frontend-work-list.md, the page gate under Handing an item); CI
// stays blind to the page by that same ruling. Exit 0 when every assertion
// holds in both browsers, 1 when one fails, 2 when the run did not happen: no
// Firefox, Chromium or chromedriver, no BiDi endpoint, no server.
//
// Usage: node page-gate.mjs <site-dir>   (Node 22+, for the global WebSocket)
//   PAGE_GATE_OVERLAYS=1 also requires an overlay submission, once 2.4 lands.
//   PAGE_GATE_BROWSERS=firefox or =chromium runs one browser alone; BROWSER,
//   CHROMIUM and CHROMEDRIVER name the binaries.
//
// What is observed, and how: a preload script wraps the globalThis hooks
// the loader installs -- lhSubmitFrame, and lhSubmitOverlays once it exists
// -- by defining them as accessor properties before loader.ts assigns them,
// so every call is counted with its size and the page's own code runs
// unchanged. The screen is read back as a hash of #screen's innerHTML, or
// of the canvas's data URL once 0.4 replaces the grid, so a key's effect
// is a stopped frame count or a changed hash rather than a claim about
// what it drew.
//
// Non-vacuity, run 2026-09-14: a site directory without LambdaHack.wasm
// fails both frame assertions with exit 1, and PAGE_GATE_OVERLAYS=1 fails
// against a tree before 2.4; the same tree passes with six frames of 80x24
// and a '?' repainting. The Chromium leg's, run 2026-10-22: the site without
// LambdaHack.wasm fails its three key and frame assertions with exit 1,
// a missing chromedriver exits 2, and a build whose syncExport drops its
// rts_scheduleWork call fails the Escape assertion, in both browsers. Run
// 2026-10-23 with the '?' press removed: the self-play assertion fails in both
// browsers, where its earlier form, a hash changed after the '?', passed in
// both, the self-play repainting regardless.

import { spawn } from "node:child_process";
import { mkdir, writeFile } from "node:fs/promises";
import { join, resolve } from "node:path";

const site = process.argv[2];
if (!site) {
  console.error("usage: node page-gate.mjs <site-dir>");
  process.exit(2);
}
const wantOverlays = process.env.PAGE_GATE_OVERLAYS === "1";
const PORT = 8123;
const DRIVER_PORT = 8124;
const BROWSER = process.env.BROWSER ?? "firefox-beta";
const CHROMIUM = process.env.CHROMIUM ?? "/usr/lib/chromium/chromium";
const CHROMEDRIVER = process.env.CHROMEDRIVER ?? "chromedriver";
const BROWSERS = (process.env.PAGE_GATE_BROWSERS ?? "firefox,chromium")
  .split(",")
  .filter(Boolean);

const children = [];
function blocked(msg) {
  console.error("BLOCKED: " + msg);
  for (const c of children) c.kill();
  process.exit(2);
}
function fail(msg) {
  console.error("FAIL: " + msg);
  process.exitCode = 1;
}
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// --- static server, the bundle's own serve.mjs -------------------------
const server = spawn("node", [join(site, "serve.mjs"), site, String(PORT)], {
  stdio: ["ignore", "pipe", "inherit"],
});
children.push(server);
await new Promise((ok, no) => {
  server.stdout.on("data", (d) => {
    if (String(d).includes("serving")) ok();
  });
  server.on("exit", (c) => no(new Error(`serve.mjs exited ${c}`)));
}).catch((e) => blocked(e.message));

// --- the browsers, each yielding a BiDi WebSocket ----------------------
// Headless Firefox with a scratch profile serves BiDi itself, and the
// client opens the session.
async function startFirefox() {
  const profile = resolve(site, "ff-profile");
  await mkdir(profile, { recursive: true });
  await writeFile(
    join(profile, "user.js"),
    [
      'user_pref("browser.shell.checkDefaultBrowser", false);',
      'user_pref("browser.startup.homepage_override.mstone", "ignore");',
      'user_pref("browser.aboutwelcome.enabled", false);',
      'user_pref("datareporting.policy.' +
        'dataSubmissionPolicyBypassNotification", true);',
      'user_pref("toolkit.telemetry.reportingpolicy.firstRun", false);',
      "",
    ].join("\n"),
  );
  const ff = spawn(
    BROWSER,
    [
      "--headless",
      "--no-remote",
      "--profile",
      profile,
      "--remote-debugging-port=0",
      "about:blank",
    ],
    { stdio: ["ignore", "pipe", "pipe"] },
  );
  children.push(ff);
  ff.on("error", (e) => blocked(`cannot start ${BROWSER}: ${e.message}`));
  const wsUrl = await new Promise((ok) => {
    const t = setTimeout(() => ok(null), 30000);
    const look = (d) => {
      const m = /WebDriver BiDi listening on (ws:\/\/\S+)/.exec(String(d));
      if (m) {
        clearTimeout(t);
        ok(m[1]);
      }
    };
    ff.stdout.on("data", look);
    ff.stderr.on("data", look);
  });
  if (!wsUrl) blocked(`${BROWSER} printed no BiDi endpoint within 30s`);
  return {
    url: wsUrl + "/session",
    newSession: true,
    stop: async () => {
      ff.kill();
    },
  };
}

// Headless Chromium serves no BiDi of its own: chromedriver starts it and
// hands back a WebSocket bound to the session it opened.
async function startChromium() {
  const driver = spawn(CHROMEDRIVER, [`--port=${DRIVER_PORT}`], {
    stdio: "ignore",
  });
  children.push(driver);
  driver.on("error", (e) =>
    blocked(`cannot start ${CHROMEDRIVER}: ${e.message}`),
  );
  const base = `http://127.0.0.1:${DRIVER_PORT}`;
  let ready = false;
  for (let i = 0; i < 60 && !ready; i++) {
    await sleep(250);
    ready = await fetch(base + "/status")
      .then((r) => r.json())
      .then(
        (j) => j.value?.ready === true,
        () => false,
      );
  }
  if (!ready) blocked(`${CHROMEDRIVER} not ready within 15s`);
  const args = [
    "--headless=new",
    "--no-first-run",
    "--no-default-browser-check",
    "--password-store=basic",
    `--user-data-dir=${resolve(site, "chromium-profile")}`,
  ];
  const capabilities = {
    alwaysMatch: {
      browserName: "chrome",
      webSocketUrl: true,
      "goog:chromeOptions": { binary: CHROMIUM, args },
    },
  };
  const res = await fetch(base + "/session", {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ capabilities }),
  }).then(
    (r) => r.json(),
    (e) => ({ error: String(e) }),
  );
  const url = res.value?.capabilities?.webSocketUrl;
  if (!url) {
    blocked(
      `${CHROMEDRIVER} opened no BiDi session: ` +
        JSON.stringify(res).slice(0, 300),
    );
  }
  return {
    url,
    newSession: false,
    stop: async () => {
      await fetch(`${base}/session/${res.value.sessionId}`, {
        method: "DELETE",
      }).catch(() => {});
      driver.kill();
    },
  };
}

// --- a minimal BiDi client -------------------------------------------
async function connect(url) {
  const ws = new WebSocket(url);
  await new Promise((ok, no) => {
    ws.onopen = ok;
    ws.onerror = () => no(new Error("websocket"));
  }).catch((e) => blocked(e.message));
  let nextId = 1;
  const pending = new Map();
  ws.onmessage = (ev) => {
    const m = JSON.parse(ev.data);
    if (m.id && pending.has(m.id)) {
      pending.get(m.id)(m);
      pending.delete(m.id);
    }
  };
  function send(method, params) {
    return new Promise((ok) => {
      const id = nextId++;
      pending.set(id, ok);
      ws.send(JSON.stringify({ id, method, params }));
    }).then((m) => {
      if (m.type === "error") {
        throw new Error(`${method}: ${m.error} ${m.message}`);
      }
      return m.result;
    });
  }
  return { send, close: () => ws.close() };
}

const HASH = `(() => { const el = document.getElementById("screen");
  const c = el && el.querySelector("canvas");
  const s = c ? c.toDataURL() : (el ? el.innerHTML : ""); let h = 0;
  for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) | 0;
  return String(h) + ":" + s.length; })()`;
async function gate(name, send) {
  const tree = await send("browsingContext.getTree", {});
  const context = tree.contexts[0].context;
  await send("script.addPreloadScript", {
    functionDeclaration: `() => {
    const g = { frames: 0, sizes: [], overlays: 0 };
    globalThis.__lhGate = g;
    let frame; let overlay;
    Object.defineProperty(globalThis, "lhSubmitFrame", { configurable: true,
      get() { return frame; },
      set(f) {
        frame = (a, w, h) => { g.frames++; g.sizes.push([w, h]);
                               return f(a, w, h); };
      } });
    Object.defineProperty(globalThis, "lhSubmitOverlays", {
      configurable: true,
      get() { return overlay; },
      set(f) { overlay = (...xs) => { g.overlays++; return f(...xs); }; } });
  }`,
  });
  async function evaluate(expression) {
    const r = await send("script.evaluate", {
      expression,
      target: { context },
      awaitPromise: true,
      resultOwnership: "none",
    });
    if (r.type !== "success") {
      throw new Error(`evaluate failed: ${JSON.stringify(r).slice(0, 300)}`);
    }
    return r.result.value;
  }
  const count = () => evaluate("globalThis.__lhGate.frames");
  const press = (value) =>
    send("input.performActions", {
      context,
      actions: [
        {
          type: "key",
          id: "kbd",
          actions: [
            { type: "keyDown", value },
            { type: "keyUp", value },
          ],
        },
      ],
    });
  await send("browsingContext.navigate", {
    context,
    url: `http://localhost:${PORT}/`,
    wait: "complete",
  });
  // 1. frames arrive after lhStart.
  let frames = 0;
  for (let i = 0; i < 120 && frames < 2; i++) {
    await sleep(500);
    frames = await count();
  }
  const size = await evaluate(
    "JSON.stringify(globalThis.__lhGate.sizes[0] ?? null)",
  );
  if (frames < 2) {
    fail(`${name}: no second frame within 60s of load (frames=${frames})`);
  } else {
    console.log(
      `ok   ${name}: frames arrive: ${frames} within the wait,` +
        ` the first ${size} cells`,
    );
  }
  // 2. a key stops the self-play. With no save the game is the insert-coin
  // screensaver, repainting until a key regains control, so the screen
  // changes with or without one; what only a key does is stop it. Frames
  // arrive in the second before '?', and a second with no frame follows
  // it, the help overlay waiting for a key.
  const framesBefore = await count();
  await sleep(1000);
  const framesAtKey = await count();
  await press("?");
  let seen = await count(),
    still = 0;
  for (let i = 0; i < 40 && still < 4; i++) {
    await sleep(250);
    const f = await count();
    still = f === seen ? still + 1 : 0;
    seen = f;
  }
  if (framesAtKey === framesBefore) {
    fail(
      `${name}: no frame in the second before '?' (frames ${framesAtKey}),` +
        " so a quiet screen after it would say nothing of the key",
    );
  } else if (still < 4) {
    fail(
      `${name}: the screen never went quiet for a second within 10s` +
        ` of a '?' key (frames ${framesAtKey} -> ${seen})`,
    );
  } else {
    console.log(
      `ok   ${name}: a key stops the self-play: frames` +
        ` ${framesBefore} -> ${framesAtKey} in the second before '?',` +
        ` then none for a second at ${seen}`,
    );
  }
  // 3. a key on a quiet screen changes it: with the game idle no timer
  // re-enters the RTS scheduler, which a key delivered by a sync export once
  // waited for. Escape leaves the help.
  const quiet = await evaluate(HASH);
  await press("");
  let changed = quiet;
  for (let i = 0; i < 12 && changed === quiet; i++) {
    await sleep(250);
    changed = await evaluate(HASH);
  }
  if (still < 4) {
    fail(`${name}: no quiet screen to press Escape on`);
  } else if (changed === quiet) {
    fail(
      `${name}: a quiet screen did not change within 3s of an Escape key` +
        ` (frames ${seen})`,
    );
  } else {
    console.log(
      `ok   ${name}: a key changes a quiet screen: ${quiet}` + ` -> ${changed}`,
    );
  }
  // 4. an overlay submission, once 2.4 lands.
  const overlays = await evaluate("globalThis.__lhGate.overlays");
  if (wantOverlays) {
    if (overlays < 1) {
      fail(
        `${name}: no overlay submission after the key` +
          " (PAGE_GATE_OVERLAYS=1)",
      );
    } else console.log(`ok   ${name}: overlays submitted: ${overlays}`);
  } else {
    console.log(
      `note ${name}: overlays submitted: ${overlays}` +
        " (not required until 2.4; PAGE_GATE_OVERLAYS=1 requires" +
        " one)",
    );
  }
}

for (const name of BROWSERS) {
  const start = { firefox: startFirefox, chromium: startChromium }[name];
  if (!start) blocked(`unknown browser ${name} in PAGE_GATE_BROWSERS`);
  const browser = await start();
  const { send, close } = await connect(browser.url);
  try {
    if (browser.newSession) await send("session.new", { capabilities: {} });
    await gate(name, send);
  } catch (e) {
    fail(`${name}: ${e.message ?? e}`);
  } finally {
    close();
    await browser.stop();
  }
}
server.kill();
process.exit(process.exitCode ?? 0);
