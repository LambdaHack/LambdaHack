import { describe, it, expect } from "vitest";
import { contentType } from "./serve-core.js";

describe("contentType", () => {
  it("serves JS and MJS as text/javascript (so ES modules load)", () => {
    expect(contentType("/dist/bundle.js")).toBe("text/javascript; charset=utf-8");
    expect(contentType("/dist/ghc_wasm_jsffi.mjs")).toBe("text/javascript; charset=utf-8");
  });
  it("serves wasm as application/wasm", () => {
    expect(contentType("/dist/LambdaHack.wasm")).toBe("application/wasm");
  });
  it("serves html and json with their types", () => {
    expect(contentType("index.html")).toBe("text/html; charset=utf-8");
    expect(contentType("a.json")).toBe("application/json; charset=utf-8");
  });
  it("serves woff/woff2 with their font types (a wrong type here makes browsers reject @font-face)", () => {
    expect(contentType("/16x16xw.woff")).toBe("font/woff");
    expect(contentType("/16x16xw.woff2")).toBe("font/woff2");
  });
  it("is case-insensitive on the extension", () => {
    expect(contentType("X.WASM")).toBe("application/wasm");
  });
  it("falls back to octet-stream for unknown or extensionless files", () => {
    expect(contentType("data.bin")).toBe("application/octet-stream");
    expect(contentType("/no/extension/here")).toBe("application/octet-stream");
  });
  it("ignores dots in directory names", () => {
    expect(contentType("/a.b/c/file")).toBe("application/octet-stream");
  });
});
