// Pure logic for the dev static server: map a request URL to a site path, and
// a file path to its HTTP Content-Type. Correctness matters --- a browser
// refuses an ES module script served with the wrong type, so .js/.mjs MUST
// be text/javascript, and the wasm is served as application/wasm. The server
// itself (serve.ts) is thin wiring.

import { extname, normalize } from "node:path";

/**
 * The site path a request URL names: the query dropped, escapes decoded,
 * leading parent-directory steps cut and "/" mapped to index.html. Null where
 * the escapes do not decode, for which the server responds 400 rather than
 * ending on decodeURIComponent's URIError.
 */
export function sitePath(url: string): string | null {
  let raw: string;
  try {
    raw = decodeURIComponent(url.split("?")[0]);
  } catch {
    return null;
  }
  const safe = normalize(raw).replace(/^(\.\.([/\\]|$))+/, "");
  return safe === "/" || safe === "" ? "/index.html" : safe;
}

const TYPES: Record<string, string> = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".wasm": "application/wasm",
  ".json": "application/json; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".woff": "font/woff",
  ".woff2": "font/woff2",
};

/** HTTP Content-Type for a file path, by extension (case-insensitive). */
export function contentType(path: string): string {
  return TYPES[extname(path).toLowerCase()] ?? "application/octet-stream";
}
