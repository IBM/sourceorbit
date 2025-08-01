import { beforeAll, describe, expect, test } from 'vitest';
import { fromCl, toCl } from '../src/utils';

describe(`CL parsing / generating`, () => {
  test('can parse & generate correctly', () => {
    const input = `CRTSQLRPGI OBJ(SNDBX87/NEWEMP) SRCSTMF('qrpglesrc/newemp.pgm.sqlrpgle') OPTION(*EVENTF) DBGVIEW(*SOURCE) CLOSQLCSR(*ENDMOD) CVTCCSID(*JOB) COMPILEOPT('TGTCCSID(*JOB)') RPGPPOPT(*LVL2)`;

    const { command, parameters } = fromCl(input);

    expect(command).toBe('CRTSQLRPGI');
    expect(Object.keys(parameters)).toHaveLength(8);

    expect(parameters[`compileopt`]).toBe(`'TGTCCSID(*JOB)'`);

    const generated = toCl(command, parameters);

    expect(generated).toBe(input);
  });
});