import { describe, it, expect } from "vitest";
import { layoutMetrics, propCutoff, propFit } from "./overlay-core.js";
import propFitTable from "./generated/prop-fit.json";

// The rows are generated from OverlayLayout.propCutoff and propFit
// by test/OverlayLayoutUnitTests.hs: a mismatch means overlay-core.ts drifted
// from OverlayLayout.hs.
describe("propCutoff and propFit against OverlayLayout's branch table", () => {
  it.each(propFitTable.rows)(
    "halfSize=$halfSize width=$width cursorX=$cursorX" +
      " allSpace=$allSpace widthRaw=$widthRaw",
    (row) => {
      const lm = layoutMetrics(row.halfSize, row.width);
      const cur = { xPx: row.cursorX, row: 0 };
      expect(propCutoff(lm, cur)).toBe(row.stop);
      expect("x" in row).toBe(!row.stop);
      if (!row.stop) {
        const fit = propFit(lm, cur, row.allSpace, row.widthRaw);
        expect({
          x: fit.x,
          widthPx: fit.widthPx,
          marker: fit.marker !== null,
        }).toEqual({ x: row.x, widthPx: row.widthPx, marker: row.marker });
        // The pen moves on by the width drawn.
        expect(fit.next).toEqual({ xPx: fit.x + fit.widthPx, row: 0 });
      }
    },
  );
});
