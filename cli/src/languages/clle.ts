import { Module, Subroutine, Variable, File, Statement } from "vscode-clle/language";
import { SourceSymbol, SymbolReferences } from "../targets";

export function collectClReferences(relativePath: string, doc: Module): SourceSymbol[] {
  const symbols: SourceSymbol[] = [];

  const defs = doc.getDefinitions();

  const getRefs = (def: Variable|Subroutine): SymbolReferences => {
    const refs = doc.getReferences(def);

    const result: SymbolReferences = {relativePath: []};

    for (const ref of refs) {
      result[relativePath].push(ref);
    }

    return result;
  }

  for (const def of defs) {
    let newSymbol: SourceSymbol|undefined;
		if (def instanceof Variable && def.name) {
      newSymbol = {
        name: def.name.value,
        type: String(def.dataType), // RTODO: what is this?
        relativePath,
        references: getRefs(def)
      }
		}

		else if (def instanceof File && def.file) {
      newSymbol = {
        name: def.file.name,
        type: `table`, //RTODO: is table correct?
        relativePath,
        references: {}, // File's don't have refs, but children do
        external: def.file.name
      }

      // RTODO: get children of file
		}

		else if (def instanceof Subroutine && def.name) {
      newSymbol = {
        name: def.name.value,
        type: `subroutine`,
        relativePath,
        references: getRefs(def)
      }
		}

    if (newSymbol) {
      symbols.push(newSymbol);
    }
  }

  return symbols;
}