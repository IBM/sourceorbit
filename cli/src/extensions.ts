
export const rpgExtensions = [`sqlrpgle`, `rpgle`];
export const clExtensions = [`clle`, `cl`, `clp`];
export const ddsExtension = [`pf`, `lf`, `dspf`, `prtf`];
export const sqlExtensions = [`sql`, `table`, `view`, `index`, `alias`, `sqlprc`, `sqludf`, `sqludt`, `sqltrg`, `sqlalias`, `sqlseq`];
export const srvPgmExtensions = [`binder`, `bnd`];
export const cmdExtensions = [`cmd`];
export const objectExtensions = [`dtaara`, `mnucmd`, `msgf`, `dtaq`, `bnddir`];

export const allExtensions = [...rpgExtensions, ...clExtensions, ...ddsExtension, ...sqlExtensions, ...srvPgmExtensions, ...cmdExtensions, ...objectExtensions];
export const scanGlob = `**/*.{${allExtensions.join(`,`)},${allExtensions.map(e => e.toUpperCase()).join(`,`)}}`;

export const referencesFileName = `.objrefs`;