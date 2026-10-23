// Minimal static file server for the built web bundle, run by node inside the
// image (the base image ships no web server, and simple servers often mislabel
// .mjs/.wasm). Bundled by esbuild to dist/serve.mjs. Wiring; the tested
// parts are sitePath and contentType, in serve-core.ts.
//
// Usage: node serve.mjs <root-dir> [port]

import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { contentType, sitePath } from "./serve-core.js";

const root = process.argv[2] ?? ".";
const port = Number(process.argv[3] ?? "8080");

const server = createServer((req, res) => {
  const rel = sitePath(req.url ?? "/");
  if (rel === null) {
    res.writeHead(400, { "content-type": "text/plain; charset=utf-8" });
    res.end("400 bad request");
    return;
  }
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
