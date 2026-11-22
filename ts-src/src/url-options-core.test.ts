import { describe, it, expect } from "vitest";
import {
  URL_OPTIONS,
  rejectionMessage,
  urlOptions,
} from "./url-options-core.js";

describe("urlOptions (query string to the argv lhStart parses)", () => {
  it("passes nothing for an empty query, as the page always started", () => {
    expect(urlOptions("")).toEqual({ args: [], params: [], warnings: [] });
    expect(urlOptions("?")).toEqual({ args: [], params: [], warnings: [] });
  });
  it("maps each allowlisted key to its option's --key=value word", () => {
    expect(urlOptions("?fontset=dejavu&allFontsScale=1.5")).toEqual({
      args: ["--fontset=dejavu", "--allFontsScale=1.5"],
      params: ["?fontset=dejavu", "?allFontsScale=1.5"],
      warnings: [],
    });
  });
  it("orders the args by the allowlist, not by the query", () => {
    expect(urlOptions("allFontsScale=2&fontset=binary").args).toEqual([
      "--fontset=binary",
      "--allFontsScale=2",
    ]);
  });
  it("ignores an unknown key with a warning", () => {
    expect(urlOptions("?utm_source=feed&allFontsScale=2")).toEqual({
      args: ["--allFontsScale=2"],
      params: ["?allFontsScale=2"],
      warnings: ["ignoring the unknown URL parameter ?utm_source"],
    });
  });
  it("ignores a debug option the engine has but the allowlist lacks", () => {
    const r = urlOptions("?stopAfterFrames=1&benchmark=1");
    expect(r.args).toEqual([]);
    expect(r.warnings).toHaveLength(2);
  });
  it("matches keys case-sensitively, as the engine's options", () => {
    expect(urlOptions("?fontSet=dejavu").args).toEqual([]);
  });
  it("takes a duplicate's last value, with a warning", () => {
    expect(urlOptions("?fontset=binary&fontset=x&fontset=dejavu")).toEqual({
      args: ["--fontset=dejavu"],
      params: ["?fontset=dejavu"],
      warnings: [
        "the URL parameter ?fontset is given 3 times; taking the last, dejavu",
      ],
    });
  });
  it("validates no value, leaving it to the engine's parser", () => {
    expect(urlOptions("?allFontsScale=big&fontset=").args).toEqual([
      "--fontset=",
      "--allFontsScale=big",
    ]);
  });
  it("keeps a value one word, so it cannot pass an option of its own", () => {
    const r = urlOptions("?fontset=" + encodeURIComponent("x --benchmark"));
    expect(r.args).toEqual(["--fontset=x --benchmark"]);
  });
  it("decodes the value as the browser encodes it", () => {
    expect(urlOptions("?fontset=a%3Db+c").args).toEqual(["--fontset=a=b c"]);
  });
  it("holds the two keys of the record's ruling and no other", () => {
    expect([...URL_OPTIONS.keys()]).toEqual(["fontset", "allFontsScale"]);
  });
});

describe("rejectionMessage", () => {
  it("names the one parameter given", () => {
    expect(rejectionMessage(["?allFontsScale=big"])).toBe(
      "The game does not accept the URL parameter ?allFontsScale=big" +
        " in the browser's address bar. The browser console says why.",
    );
  });
  it("names every parameter given, there being no telling which failed", () => {
    expect(rejectionMessage(["?fontset=x", "?allFontsScale=2"])).toBe(
      "The game does not accept one of the URL parameters ?fontset=x," +
        " ?allFontsScale=2 in the browser's address bar. The browser console" +
        " says why.",
    );
  });
});
