// Minimal static file server for the built web bundle, run by node inside the
// image (the base image ships no web server, and simple servers often mislabel
// .mjs/.wasm). Bundled by esbuild to dist/serve.mjs. Wiring; the only tested
// part is contentType.
//
// Usage: node serve.mjs <root-dir> [port]

import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { join, normalize } from "node:path";
import { contentType } from "./serve-core.js";

const root = process.argv[2] ?? ".";
const port = Number(process.argv[3] ?? "8080");

const server = createServer((req, res) => {
  const rawPath = decodeURIComponent((req.url ?? "/").split("?")[0]);
  // Block path traversal, then map "/" to index.html.
  const safe = normalize(rawPath).replace(/^(\.\.([/\\]|$))+/, "");
  const rel = safe === "/" || safe === "" ? "/index.html" : safe;
  const file = join(root, rel);
  readFile(file)
    .then((body) => {
      res.writeHead(200, { "content-type": contentType(file) });
      res.end(body);
    })
    .catch(() => {
      res.writeHead(404, { "content-type": "text/plain; charset=utf-8" });
      res.end("404 not found");
    });
});

server.listen(port, () => {
  console.log(`serving ${root} at http://localhost:${port}`);
});
