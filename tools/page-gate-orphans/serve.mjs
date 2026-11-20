// The site of the orphan case: a blank page with the gate's #screen, each load
// of it noted as a line of the file `navigated` in the site directory, so that
// run.sh can tell when a browser has the page up. Usage, as page-gate.mjs runs
// a site's serve.mjs: node serve.mjs <site-dir> <port>
import { createServer } from "node:http";
import { appendFileSync } from "node:fs";
import { join } from "node:path";

const [site, port] = process.argv.slice(2);
createServer((req, res) => {
  if (req.url === "/") appendFileSync(join(site, "navigated"), "x\n");
  res.writeHead(200, { "content-type": "text/html" });
  res.end("<!doctype html><title>t</title><div id=screen></div>");
}).listen(Number(port), () => console.log("serving"));
