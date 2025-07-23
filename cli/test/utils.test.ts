import { describe, expect, it } from "vitest";
import { globalEntryIsValid } from "../src/utils";

describe(`util tests`, () => {
  it(`should pass`, () => {
    expect(globalEntryIsValid(`/RPGLEREPL/BND/REPL_CMPL.BND`, `QRPGLEREF/REPL_USR.*`)).toBeFalsy();
    expect(globalEntryIsValid(`/RPGLEREPL/REF/REPL_USR.RPGLEINC`, `QRPGLEREF/REPL_USR.*`)).toBeFalsy();
    expect(globalEntryIsValid(`/RPGLEREPL/BND/REPL_USR.BND`, `REPL_USR.R*`)).toBeFalsy();
    expect(globalEntryIsValid(`/RPGLEREPL/QRPGLEREF/REPL_USR.BND`, `REPL_USR.R*`)).toBeFalsy();

    expect(globalEntryIsValid(`/RPGLEREPL/QRPGLEREF/REPL_USR.RPGLEINC`, `REPL_USR.R*`)).toBeTruthy();
    expect(globalEntryIsValid(`/RPGLEREPL/QRPGLEREF/REPL_USR.RPGLEINC`, `QRPGLEREF/REPL_USR.*`)).toBeTruthy();
    expect(globalEntryIsValid(`/RPGLEREPL/BND/REPL_USR.BND`, `REPL_USR.*`)).toBeTruthy();
  });
});