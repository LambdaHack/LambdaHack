// The URL-parameter options: the page's query string translated into the argv
// lhStart's options parser reads, so that an address-bar parameter sets in the
// browser what a command-line option sets natively. Only the allowlisted keys
// pass, each as its option's --key=value word, so no value can become an
// option of its own; values are not checked here, the engine's parser
// rejecting what it cannot read. No DOM; the wiring is loader.ts.

/**
 * The query keys taken, each with the engine option it sets. A pasted link
 * must not carry a soft-brick, so no debug option is here, and each addition
 * comes with its recorded reason (docs/wasm-frontend-unified-plan.md, R4).
 */
export const URL_OPTIONS: ReadonlyMap<string, string> = new Map([
  ["fontset", "--fontset"],
  ["allFontsScale", "--allFontsScale"],
]);

/** A query string translated: what the loader passes and what it says. */
export interface UrlOptions {
  /** The argv words after the program name, in the allowlist's order. */
  args: string[];
  /** The parameters taken, as `?key=value`, to name if the game refuses. */
  params: string[];
  /** One line per ignored key and per key given more than once. */
  warnings: string[];
}

/**
 * Translate a query string, `location.search`, with or without its `?`.
 * An unknown key is ignored with a warning, and a key given more than once
 * takes its last value, with a warning too.
 */
export function urlOptions(search: string): UrlOptions {
  const values = new Map<string, string>();
  const counts = new Map<string, number>();
  const warnings: string[] = [];
  for (const [key, value] of new URLSearchParams(search)) {
    if (!URL_OPTIONS.has(key)) {
      warnings.push(`ignoring the unknown URL parameter ?${key}`);
      continue;
    }
    values.set(key, value);
    counts.set(key, (counts.get(key) ?? 0) + 1);
  }
  const args: string[] = [];
  const params: string[] = [];
  for (const [key, option] of URL_OPTIONS) {
    const value = values.get(key);
    if (value === undefined) continue;
    const n = counts.get(key) ?? 0;
    if (n > 1) {
      warnings.push(
        `the URL parameter ?${key} is given ${n} times; taking the last,` +
          ` ${value}`,
      );
    }
    args.push(`${option}=${value}`);
    params.push(`?${key}=${value}`);
  }
  return { args, params, warnings };
}

/**
 * What the page says when the game refuses to start on the URL parameters
 * given: their names and values. The reason is the engine's message
 * on the console, not its exit status.
 */
export function rejectionMessage(params: string[]): string {
  const what =
    params.length === 1 ? "the URL parameter" : "one of the URL parameters";
  return (
    `The game does not accept ${what} ${params.join(", ")}` +
    " in the browser's address bar. The browser console says why."
  );
}
