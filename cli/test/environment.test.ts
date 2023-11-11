import { assert, describe, expect, test } from 'vitest'
import { getBranchLibraryName } from '../src/builders/environment';

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