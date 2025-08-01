import { assert, describe, expect, test } from 'vitest'
import { getBranchLibraryName } from '../src/builders/environment';
import { fromCl, getReferenceObjectsFrom, getSystemNameFromPath } from '../src/utils';

describe(`Deterministic branch name`, () => {
  test('Basic name', () => {
    expect(getBranchLibraryName(`mybranch`)).toBe(`VSFE4DD065`);
  });

  test('Long name', () => {
    expect(getBranchLibraryName(`superlongname`)).toBe(`VS1DB7F840`);
  });

  test('Long name with space', () => {
    expect(getBranchLibraryName(`superlong name`)).toBe(`VSF1A2FCF7`);
  });

  test('Type with description (short)', () => {
    expect(getBranchLibraryName(`feature/cool`)).toBe(`VSC4B5B445`);
  });

  test('Type with description (longer)', () => {
    expect(getBranchLibraryName(`feature/coolidea`)).toBe(`VS826FEB8F`);
  });

  test('Type with description (longer with gaps)', () => {
    expect(getBranchLibraryName(`feature/coolidea-thingy`)).toBe(`VSB76F6BBB`);
  });

  test('Type with description (longer, id at start)', () => {
    expect(getBranchLibraryName(`feature/123-coolidea-thingy`)).toBe(`VSFD17325E`);
  });

  test('Type with description (longer, id at end)', () => {
    expect(getBranchLibraryName(`feature/coolidea-thingy-1234`)).toBe(`VSC94DBD96`);
  });

  test('Type with description (longer, id too long)', () => {
    expect(getBranchLibraryName(`feature/coolidea-thingy-12342346`)).toBe(`VS4895C408`);
  });
});

describe(`Deterministic system name`, () => {
  test('Basic name', () => {
    expect(getSystemNameFromPath(`abcd`)).toBe(`ABCD`);
  })
  test('Basic name with underscore', () => {
    expect(getSystemNameFromPath(`ab_cd`)).toBe(`AB_CD`);
  })
  test('Long form lowercase', () => {
    expect(getSystemNameFromPath(`thisisasuperlongname`)).toBe(`THISISASUP`);
  })
  test('Long form uppercase', () => {
    expect(getSystemNameFromPath(`THISISASUPERLONGNAME`)).toBe(`THISISASUP`);
  })
  test('Long form camel case', () => {
    expect(getSystemNameFromPath(`thisIsASuperLongName`)).toBe(`TIASLN`);
  })
  test('With capitals', () => {
    expect(getSystemNameFromPath(`FetchUserData`)).toBe(`FUD`);
  })
  test('With underscore', () => {
    expect(getSystemNameFromPath(`ua_fetchUserData`)).toBe(`UAFUD`);
  })
  test('With long underscore', () => {
    expect(getSystemNameFromPath(`abcdefhijkl_fetchUserData`)).toBe(`ABCDEFHIJK`);
  })
  test('Bob prefix name A', () => {
    expect(getSystemNameFromPath(`ART200-Work_with_article`)).toBe(`ART200`);
  })
  test('Bob prefix name B', () => {
    expect(getSystemNameFromPath(`ART200D-Work_with_Article`)).toBe(`ART200D`);
  })
});

describe(`Deterministic test system name`, () => {
  test('Basic name', () => {
    expect(getSystemNameFromPath(`abcd.test`)).toBe(`TABCD`);
  })
  test('Basic name with underscore', () => {
    expect(getSystemNameFromPath(`ab_cd.test`)).toBe(`TAB_CD`);
  })
  test('Long form lowercase', () => {
    expect(getSystemNameFromPath(`thisisasuperlongname.test`)).toBe(`TTHISISASU`);
  })
  test('Long form uppercase', () => {
    expect(getSystemNameFromPath(`THISISASUPERLONGNAME.test`)).toBe(`TTHISISASU`);
  })
  test('Long form camel case', () => {
    expect(getSystemNameFromPath(`thisIsASuperLongName.test`)).toBe(`TTIASLN`);
  })
  test('With capitals', () => {
    expect(getSystemNameFromPath(`FetchUserData.test`)).toBe(`TFUD`);
  })
  test('With underscore', () => {
    expect(getSystemNameFromPath(`ua_fetchUserData.test`)).toBe(`TUAFUD`);
  })
  test('With long underscore', () => {
    expect(getSystemNameFromPath(`abcdefhijkl_fetchUserData.test`)).toBe(`TABCDEFHIJ`);
  })
  test('Bob prefix name A', () => {
    expect(getSystemNameFromPath(`ART200-Work_with_article.test`)).toBe(`TART200`);
  })
  test('Bob prefix name B', () => {
    expect(getSystemNameFromPath(`ART200D-Work_with_Article.test`)).toBe(`TART200D`);
  })
});

describe(`CL parser`, () => {
  test('Basic command', () => {
    const cl = `CRTCLPGM PGM(MYLIB/MYCL) SRCFILE(MYLIB/QCLSRC) SRCMBR(MYCL)`;
    const parsed = fromCl(cl);

    expect(parsed.command).toBe(`CRTCLPGM`);
    expect(parsed.parameters).toMatchObject({
      pgm: `MYLIB/MYCL`,
      srcfile: `MYLIB/QCLSRC`,
      srcmbr: `MYCL`
    });
  });

  test('Just command', () => {
    const cl = `CRTCLPGM`;
    const parsed = fromCl(cl);

    expect(parsed.command).toBe(`CRTCLPGM`);
    expect(parsed.parameters).toMatchObject({});
  });
});

describe(`Reference files`, () => {
  test('content', () => {
    const lines = [
      `# Files that exist outside this project`,
      ``,
      `COUNTRY.FILE`,
      ``,
      `# Service programs outside this project`,
      `UTILS.SRVPGM`,
      `  toUpper`,
      `  toLower`,
    ].join('\n');

    const refObjects = getReferenceObjectsFrom(lines);

    expect(refObjects).toHaveLength(2);

    expect(refObjects[0]).toMatchObject({
      type: `FILE`,
      systemName: `COUNTRY`
    });

    expect(refObjects[1]).toMatchObject({
      type: `SRVPGM`,
      systemName: `UTILS`,
      functions: [{
        name: `TOUPPER`,
        export: true,
        lineRange: [6, 6]
      }, {
        name: `TOLOWER`,
        export: true,
        lineRange: [7, 7]
      }],
    });
  });
})