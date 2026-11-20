// The deployed-page gate: build outputs in a directory, served here and loaded
// in headless Firefox and headless Chromium over WebDriver BiDi, Chromium's
// through chromedriver, must boot the reactor, paint, draw the map's font
// without subpixel antialiasing at 1:1 and 2:1, stop its self-play on a key,
// repaint on a key on a quiet screen and submit overlays, and, loaded again
// without the overlay fonts' files, warn of a font that failed and still draw
// overlay text, in each browser. The two run side by side, so their lines
// interleave, each naming its browser. It is the one check in the repository
// that asks whether the page players load works, and it runs locally, before
// every browser landing, by ruling (docs/wasm-frontend-work-list.md, the
// page gate under Handing an item); CI stays blind to the page by that same
// ruling. Exit 0 when every assertion holds in both browsers, 1 when one
// fails, 2 when the run did not happen: no browser named or an unknown one,
// no Firefox, Chromium or chromedriver, no BiDi endpoint or one that closes
// or stops answering, no server, no bundle of the generated palette, a throw
// outside the assertions, a SIGINT or SIGTERM; a FAIL already printed keeps 1.
//
// Usage: node page-gate.mjs <site-dir> (Node 22+, for the global WebSocket)
//   PAGE_GATE_BROWSERS=firefox or =chromium runs one browser alone; BROWSER,
//   CHROMIUM and CHROMEDRIVER name the binaries.
//
// What is observed, and how: a preload script wraps the globalThis hooks the
// loader installs -- lhSubmitFrame and lhSubmitOverlays -- by defining them as
// accessor properties before loader.ts assigns them, so every call is counted,
// the first with its size, and the page's own code runs unchanged; it replaces
// console.warn and the 2D context's fillText with wrappers that keep the
// warnings and count the fillText calls made while the context's font names an
// overlay family. The screen is read back as a hash of the canvas's data URL,
// so a key's effect is a stopped frame count or a changed hash rather than a
// claim about what it drew; the HUD rows' pixels are read back too, against the
// generated palette.
//
// Non-vacuity: the gate's controls are mutants in tools/mutants-deep.py,
// each judged by `make page-gate`, or those of its exit handling by
// tools/page-gate-orphans/run.sh, and replayed by `selftest-mutants.py
// tools/mutants-deep.py`.

import { spawn } from "node:child_process";
import { mkdir, readdir, rm, symlink, writeFile } from "node:fs/promises";
import { join, resolve } from "node:path";

import { importTs, sleep, waitFor, waitQuiet } from "./run-wasm-test.mjs";

const site = process.argv[2];
if (!site) {
  console.error("usage: node page-gate.mjs <site-dir>");
  process.exit(2);
}
const PORT = 8123;
const DRIVER_PORT = 8124;
const BROWSER = process.env.BROWSER ?? "firefox-beta";
const CHROMIUM = process.env.CHROMIUM ?? "/usr/lib/chromium/chromium";
const CHROMEDRIVER = process.env.CHROMEDRIVER ?? "chromedriver";
const BROWSERS = (process.env.PAGE_GATE_BROWSERS ?? "firefox,chromium")
  .split(",")
  .filter(Boolean);
// How long a BiDi command may go unanswered.
const COMMAND_MS = 60000;

// Every child starts in a process group of its own and is killed with its
// group at exit, whichever verdict ends the run, so that no server or browser
// outlives the gate holding its port. Chromium, which chromedriver starts,
// stays in chromedriver's group, and the DELETE that closes it runs only where
// a run reaches stop(); the exit handler is synchronous, so a signal is all it
// can send.
const children = [];
function start(command, args, options) {
  const child = spawn(command, args, { ...options, detached: true });
  children.push(child);
  return child;
}
process.on("exit", () => {
  for (const c of children) {
    try {
      process.kill(-c.pid, "SIGTERM");
    } catch {
      // never started, or its group already gone
    }
  }
});
function blocked(msg) {
  console.error("BLOCKED: " + msg);
  process.exit(process.exitCode === 1 ? 1 : 2);
}
function fail(msg) {
  console.error("FAIL: " + msg);
  process.exitCode = 1;
}
process.on("uncaughtException", (e) => blocked(`uncaught: ${e?.stack ?? e}`));
// The children are out of the terminal's process group, so an interrupt reaches
// the gate alone, which ends the run as any other exit does.
for (const s of ["SIGINT", "SIGTERM"]) process.on(s, () => blocked(s));
if (BROWSERS.length === 0) blocked("PAGE_GATE_BROWSERS names no browser");
// What starts each browser PAGE_GATE_BROWSERS may name.
const STARTERS = { firefox: startFirefox, chromium: startChromium };
for (const name of BROWSERS) {
  if (!Object.hasOwn(STARTERS, name)) {
    blocked(`unknown browser ${name} in PAGE_GATE_BROWSERS`);
  }
}

// The site again, under it, without the overlay fonts' files.
const MISSING = "missing-fonts";
await rm(join(site, MISSING), { recursive: true, force: true });
await mkdir(join(site, MISSING));
for (const f of await readdir(site, { withFileTypes: true })) {
  if (f.isFile() && !f.name.endsWith(".ttf.woff")) {
    await symlink(resolve(site, f.name), join(site, MISSING, f.name));
  }
}

// --- static server, the bundle's own serve.mjs -------------------------
const server = start("node", [join(site, "serve.mjs"), site, String(PORT)], {
  stdio: ["ignore", "pipe", "inherit"],
});
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
  const ff = start(
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

// Headless Chromium serves no BiDi of its own: chromedriver starts it and hands
// back a WebSocket bound to the session it opened.
async function startChromium() {
  const driver = start(CHROMEDRIVER, [`--port=${DRIVER_PORT}`], {
    stdio: "ignore",
  });
  driver.on("error", (e) =>
    blocked(`cannot start ${CHROMEDRIVER}: ${e.message}`),
  );
  const base = `http://127.0.0.1:${DRIVER_PORT}`;
  const ready = await waitFor(
    () =>
      fetch(base + "/status")
        .then((r) => r.json())
        .then(
          (j) => j.value?.ready === true,
          () => false,
        ),
    15000,
    250,
  );
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
  // A browser that dies mid-gate closes the socket, and one that hangs answers
  // nothing; either way the gate cannot go on.
  ws.onclose = () => blocked("the BiDi connection closed mid-gate");
  function send(method, params) {
    return new Promise((ok) => {
      const id = nextId++;
      const timer = setTimeout(
        () => blocked(`${method}: no reply within ${COMMAND_MS / 1000}s`),
        COMMAND_MS,
      );
      pending.set(id, (m) => {
        clearTimeout(timer);
        ok(m);
      });
      ws.send(JSON.stringify({ id, method, params }));
    }).then((m) => {
      if (m.type === "error") {
        throw new Error(`${method}: ${m.error} ${m.message}`);
      }
      return m.result;
    });
  }
  return {
    send,
    close: () => {
      ws.onclose = null;
      ws.close();
    },
  };
}

// The colours the game draws in, the generated palette's, the backlight fill
// included, and the backgrounds a glyph is drawn over: black and that fill.
const { PALETTE: HEX, BACKLIGHT_FILL } = await importTs(
  "src/generated/palette.ts",
);
const hex = (s) => parseInt(s.slice(1), 16);
const PALETTE = [...HEX, BACKLIGHT_FILL].map(hex);
const BACKGROUNDS = [0, hex(BACKLIGHT_FILL)];
const rgb = (v) => [(v >> 16) & 255, (v >> 8) & 255, v & 255];
// Whether a colour lies, within 3 per channel, on the segment from a palette
// colour to a background, as the greyscale coverage of a glyph's edge does;
// subpixel antialiasing's fringes are tints off every such segment.
function isBlend(v) {
  const p = rgb(v);
  return PALETTE.some((fg) =>
    BACKGROUNDS.some((bg) => {
      const f = rgb(fg);
      const b = rgb(bg);
      const d = f.map((x, i) => x - b[i]);
      const n = d.reduce((s, x) => s + x * x, 0);
      if (n === 0) return false;
      const t = d.reduce((s, x, i) => s + (p[i] - b[i]) * x, 0) / n;
      const k = Math.min(1, Math.max(0, t));
      return d.every((x, i) => Math.abs(p[i] - (b[i] + k * x)) <= 3);
    }),
  );
}
// The HUD's two bottom rows, all in the map font: how many of their device
// pixels are lit, and the colours of those of no palette colour with their
// counts.
const HUD_PIXELS = `(() => {
  const ok = new Set(${JSON.stringify(PALETTE)});
  const c = document.getElementById("screen").querySelector("canvas");
  const ch = c.height / globalThis.__lhGate.first[1];
  const y = Math.round(c.height - 2 * ch);
  const d = c.getContext("2d").getImageData(0, y, c.width, c.height - y).data;
  let lit = 0;
  const off = {};
  for (let i = 0; i < d.length; i += 4) {
    if (d[i] + d[i + 1] + d[i + 2] > 0) lit++;
    const v = (d[i] << 16) | (d[i + 1] << 8) | d[i + 2];
    if (!ok.has(v)) off[v] = (off[v] ?? 0) + 1;
  }
  return JSON.stringify({ lit, off });
})()`;
// The canvas's width, in device pixels.
const CANVAS_WIDTH =
  'document.getElementById("screen").querySelector("canvas").width';
const HASH = `(() => { const el = document.getElementById("screen");
  const s = el?.querySelector("canvas")?.toDataURL() ?? ""; let h = 0;
  for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) | 0;
  return String(h) + ":" + s.length; })()`;
async function gate(name, send) {
  const tree = await send("browsingContext.getTree", {});
  const context = tree.contexts[0].context;
  await send("script.addPreloadScript", {
    functionDeclaration: `() => {
    const g = { frames: 0, first: null, overlays: 0, warnings: [], texts: 0 };
    globalThis.__lhGate = g;
    const warn = console.warn;
    console.warn = (...xs) => { g.warnings.push(String(xs[0]));
                                return warn.apply(console, xs); };
    const fillText = CanvasRenderingContext2D.prototype.fillText;
    CanvasRenderingContext2D.prototype.fillText = function (...xs) {
      if (/lambdaHack(Prop|Mono)/.test(this.font)) g.texts++;
      return fillText.apply(this, xs);
    };
    let frame; let overlay;
    Object.defineProperty(globalThis, "lhSubmitFrame", { configurable: true,
      get() { return frame; },
      set(f) {
        frame = (a, w, h) => { g.frames++; g.first ??= [w, h];
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
  await waitFor(async () => (frames = await count()) >= 2, 60000, 500);
  const size = await evaluate("JSON.stringify(globalThis.__lhGate.first)");
  if (frames < 2) {
    fail(`${name}: no second frame within 60s of load (frames=${frames})`);
  } else {
    console.log(
      `ok   ${name}: frames arrive: ${frames} within the wait,` +
        ` the first ${size} cells`,
    );
  }
  // 2. no subpixel antialiasing in the map's font, at 1:1 and, the page laid
  // out again at device pixel ratio 2, at 2:1: of the self-play's HUD rows,
  // every lit pixel a palette colour or a blend of one with the background,
  // never a tint (terminal-02). Blends are counted, not failed: the outline
  // woff's points sit off the pixel grid, and only a pixel-exact font removes
  // them, as the record's ruling of 2026-10-26 under R7 asks.
  const crisp = async (scale) => {
    const r = JSON.parse(await evaluate(HUD_PIXELS));
    let blends = 0;
    let tints = 0;
    for (const [v, n] of Object.entries(r.off)) {
      if (isBlend(Number(v))) blends += n;
      else tints += n;
    }
    if (r.lit < 200 * scale * scale) {
      fail(
        `${name}: at ${scale}:1 the HUD rows hold ${r.lit} lit pixels,` +
          " too few to judge the font by",
      );
    } else if (tints > 0) {
      fail(
        `${name}: at ${scale}:1 ${tints} of the HUD rows' ${r.lit} lit` +
          " pixels are tints no palette colour blends to: the map's font" +
          " has subpixel antialiasing",
      );
    } else {
      console.log(
        `ok   ${name}: no subpixel antialiasing at ${scale}:1: of ${r.lit}` +
          ` lit HUD pixels ${blends} are blends of a palette colour with` +
          " the background, none a tint",
      );
    }
  };
  await crisp(1);
  const widthNow = () => evaluate(CANVAS_WIDTH);
  const width1 = await widthNow();
  await send("browsingContext.setViewport", { context, devicePixelRatio: 2 });
  let width2 = width1;
  await waitFor(
    async () => (width2 = await widthNow()) === 2 * width1,
    10000,
    250,
  );
  if (width2 !== 2 * width1) {
    fail(
      `${name}: at device pixel ratio 2 the canvas stayed ${width2} wide,` +
        ` not ${2 * width1}`,
    );
  } else await crisp(2);
  await send("browsingContext.setViewport", {
    context,
    devicePixelRatio: null,
  });
  await waitFor(async () => (await widthNow()) === width1, 10000, 250);
  // 3. a key stops the self-play. With no save the game is the insert-coin
  // screensaver, repainting until a key regains control, so the screen
  // changes with or without one; what only a key does is stop it. Frames
  // arrive in the second before '?', and a second with no frame follows
  // it, the help overlay waiting for a key.
  const framesBefore = await count();
  await sleep(1000);
  const framesAtKey = await count();
  let seen = framesAtKey;
  let stopped = false;
  if (framesAtKey === framesBefore) {
    fail(
      `${name}: no frame in the second before '?' (frames ${framesAtKey}),` +
        " so a quiet screen after it would say nothing of the key",
    );
  } else {
    await press("?");
    stopped = await waitQuiet(
      async () => (seen = await count()),
      1000,
      10000,
      250,
    );
    if (!stopped) {
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
  }
  // 4. a key on a quiet screen changes it: with the game idle no timer
  // re-enters the RTS scheduler, which a key delivered by a sync export once
  // waited for. Escape leaves the help.
  if (!stopped) {
    fail(`${name}: no quiet screen to press Escape on`);
  } else {
    const quiet = await evaluate(HASH);
    await press("\uE00C");
    let changed = quiet;
    await waitFor(
      async () => (changed = await evaluate(HASH)) !== quiet,
      3000,
      250,
    );
    if (changed === quiet) {
      fail(
        `${name}: a quiet screen did not change within 3s of an Escape key` +
          ` (frames ${seen})`,
      );
    } else {
      console.log(
        `ok   ${name}: a key changes a quiet screen: ${quiet} -> ${changed}`,
      );
    }
  }
  // 5. an overlay submission.
  const overlays = await evaluate("globalThis.__lhGate.overlays");
  if (overlays < 1) {
    fail(`${name}: no overlay submission after the key`);
  } else console.log(`ok   ${name}: overlays submitted: ${overlays}`);
  // 6. fonts that fail to load: loaded again without the overlay fonts' files,
  // the page warns of a face that failed and still draws overlay text, the
  // frames held for those faces released as their loads fail.
  await send("browsingContext.navigate", {
    context,
    url: `http://localhost:${PORT}/${MISSING}/index.html`,
    wait: "complete",
  });
  let after = { warnings: [], texts: 0 };
  await waitFor(
    async () => {
      after = JSON.parse(
        await evaluate(
          "JSON.stringify({ warnings: globalThis.__lhGate.warnings" +
            ".filter((w) => w.startsWith('[lh] font ')), texts:" +
            " globalThis.__lhGate.texts })",
        ),
      );
      return after.warnings.length > 0 && after.texts > 0;
    },
    30000,
    500,
  );
  if (after.warnings.length === 0) {
    fail(`${name}: no font warning within 30s of a load missing the fonts`);
  } else if (after.texts === 0) {
    fail(
      `${name}: no overlay text drawn within 30s of a load missing the` +
        ` fonts, after ${after.warnings.length} font warnings`,
    );
  } else {
    console.log(
      `ok   ${name}: with the overlay fonts missing, ` +
        `${after.warnings.length} font warnings and ${after.texts} overlay` +
        " texts drawn",
    );
  }
}

await Promise.all(
  BROWSERS.map(async (name) => {
    const browser = await STARTERS[name]();
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
  }),
);
process.exit(process.exitCode ?? 0);
