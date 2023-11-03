import { assert, describe, expect, test } from 'vitest'
import { getBranchLibraryName } from '../src/builders/environment';

describe(`Deterministic branch name`, () => {
  test('Basic name', () => {
    expect(getBranchLibraryName(`mybranch`)).toBe(`MYBRANCH`);
  });

  test('Long name', () => {
    expect(getBranchLibraryName(`superlongname`)).toBe(`SUPERLONGN`);
  });

  test('Long name with space', () => {
    expect(getBranchLibraryName(`superlong name`)).toBe(`SUPNAME`);
  });

  test('Type with description (short)', () => {
    expect(getBranchLibraryName(`feature/cool`)).toBe(`FEACOOL`);
  });

  test('Type with description (longer)', () => {
    expect(getBranchLibraryName(`feature/coolidea`)).toBe(`FEACOOLIDE`);
  });

  test('Type with description (longer with gaps)', () => {
    expect(getBranchLibraryName(`feature/coolidea-thingy`)).toBe(`FEACOOLIDE`);
  });

  test('Type with description (longer, id at start)', () => {
    expect(getBranchLibraryName(`feature/123-coolidea-thingy`)).toBe(`FEA123`);
  });

  test('Type with description (longer, id at end)', () => {
    expect(getBranchLibraryName(`feature/coolidea-thingy-1234`)).toBe(`FEA1234`);
  });

  test('Type with description (longer, id too long)', () => {
    expect(getBranchLibraryName(`feature/coolidea-thingy-12342346`)).toBe(`FEACOOLIDE`);
  });
});