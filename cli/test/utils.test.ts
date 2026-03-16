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

  it(`should handle mixed path separators (cross-platform fix)`, () => {
    // Windows-style paths with forward slashes (glob results on Windows)
    expect(globalEntryIsValid(
      `C:/Users/user/project/qrpgleref/empdet.rpgleinc`,
      `QRPGLEREF/EMPDET.RPGLEINC`
    )).toBeTruthy();

    // Windows-style paths with backslashes - this is the key fix
    expect(globalEntryIsValid(
      `C:\\Users\\user\\project\\qrpgleref\\empdet.rpgleinc`,
      `QRPGLEREF/EMPDET.RPGLEINC`
    )).toBeTruthy();

    // Windows-style paths with mixed separators
    expect(globalEntryIsValid(
      `C:\\Users\\user/project\\qrpgleref/empdet.rpgleinc`,
      `QRPGLEREF/EMPDET.RPGLEINC`
    )).toBeTruthy();

    // Unix-style paths with forward slashes
    expect(globalEntryIsValid(
      `/home/user/project/qrpgleref/empdet.rpgleinc`,
      `QRPGLEREF/EMPDET.RPGLEINC`
    )).toBeTruthy();

    // Unix-style paths with backslashes (edge case, but should still work)
    expect(globalEntryIsValid(
      `/home/user/project/qrpgleref\\empdet.rpgleinc`,
      `QRPGLEREF/EMPDET.RPGLEINC`
    )).toBeTruthy();

    // Non-matching filenames should return false (Windows)
    expect(globalEntryIsValid(
      `C:\\Users\\user\\project\\qrpgleref\\different.rpgleinc`,
      `QRPGLEREF/EMPDET.RPGLEINC`
    )).toBeFalsy();

    // Non-matching filenames should return false (Unix)
    expect(globalEntryIsValid(
      `/home/user/project/qrpgleref/different.rpgleinc`,
      `QRPGLEREF/EMPDET.RPGLEINC`
    )).toBeFalsy();

    // Wildcard matching with backslashes (Windows)
    expect(globalEntryIsValid(
      `C:\\Users\\user\\project\\qrpgleref\\empdet.rpgleinc`,
      `QRPGLEREF/EMPDET.*`
    )).toBeTruthy();

    // Wildcard matching with forward slashes (Unix)
    expect(globalEntryIsValid(
      `/home/user/project/qrpgleref/empdet.rpgleinc`,
      `QRPGLEREF/EMPDET.*`
    )).toBeTruthy();

    // Case insensitivity with backslashes (Windows)
    expect(globalEntryIsValid(
      `C:\\Users\\user\\project\\QRPGLEREF\\EMPDET.RPGLEINC`,
      `qrpgleref/empdet.rpgleinc`
    )).toBeTruthy();

    // Case insensitivity with forward slashes (Unix)
    expect(globalEntryIsValid(
      `/home/user/project/QRPGLEREF/EMPDET.RPGLEINC`,
      `qrpgleref/empdet.rpgleinc`
    )).toBeTruthy();
  });
});