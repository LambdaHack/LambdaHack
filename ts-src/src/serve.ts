// Minimal static file server for the built web bundle, run by
// `make serve-wasm` and by page-gate.mjs, since simple servers often mislabel
// .mjs/.wasm. Bundled by esbuild to dist/serve.mjs. Wiring; the tested parts
// are sitePath and contentType, in serve-core.ts.
//
// Usage: node serve.mjs <root-dir> [port]

import { createReadStream } from "node:fs";
import { stat } from "node:fs/promises";
import { createServer } from "node:http";
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
  // Streamed rather than read whole, so that the first bytes leave at
  // once, the wasm's among them, which the loader compiles as they come.
  const file = join(root, rel);
  stat(file)
    .then((st) => {
      if (!st.isFile()) throw new Error(`${file} is not a file`);
      res.writeHead(200, {
        "content-type": contentType(file),
        "content-length": st.size,
      });
      createReadStream(file)
        .on("error", () => res.destroy())
        .pipe(res);
    })
    .catch(() => {
      res.writeHead(404, { "content-type": "text/plain; charset=utf-8" });
      res.end("404 not found");
    });
});

server.listen(port, () => {
  console.log(`serving ${root} at http://localhost:${port}`);
});
