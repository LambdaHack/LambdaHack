// Pure logic for the dev static server: map a file path to its HTTP
// Content-Type. Correctness matters — a browser refuses an ES module script
// served with the wrong type, so .js/.mjs MUST be text/javascript, and the wasm
// is served as application/wasm. The server itself (serve.ts) is thin wiring.

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
  const dot = path.lastIndexOf(".");
  const slash = Math.max(path.lastIndexOf("/"), path.lastIndexOf("\\"));
  const ext = dot > slash ? path.slice(dot).toLowerCase() : "";
  return TYPES[ext] ?? "application/octet-stream";
}
