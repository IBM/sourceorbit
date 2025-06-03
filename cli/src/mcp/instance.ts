import { McpServer } from "@modelcontextprotocol/sdk/server/mcp";
import { Targets } from "../targets";
import { z } from "zod";

const OBJECT_TYPES = z.enum([`PGM`, `SRVPGM`, `MODULE`, `FILE`, `BNDDIR`, `DTAARA`, `CMD`, `MENU`, `DTAQ`])

export function createMcpInstance(targets: Targets) {
  const server = new McpServer({
    name: "Source Orbit",
    version: "1.0.0"
  });

  server.tool("findObjectsByName", "Find objects by name", 
    { name: z.string() },
    async ({ name }) => ({
      content: [{ type: "text", text: JSON.stringify(
        targets.getResolvedObjects().filter(obj => obj.systemName.toUpperCase() === name.toUpperCase() || obj.longName.toUpperCase() === name.toUpperCase())
      )}]
    })
  );

  server.tool("findObjectsByType", "Find objects by type and name", 
    { name: z.string().optional(), type: OBJECT_TYPES },
    async ({ name, type }) => ({
      content: [{ type: "text", text: JSON.stringify(
        targets.getResolvedObjects(type).filter(obj => name === undefined || (obj.systemName.toUpperCase().includes(name.toUpperCase()) || obj.longName.toUpperCase().includes(name.toUpperCase())))
      )}]
    })
  );

  // TODO: object dependency lookup
  // TODO: object impacts

  server.tool("getAllExports", "Get all export function/procedure names",
    {},
    async () => {
      const exports = Object.keys(targets.getExports());
      return {content: [{ type: "text", text: JSON.stringify(exports, null, 2) }]};
    }
  )

  server.tool("lookupExportDefinition", "Find where an export function/procedure is defined", 
    { name: z.string() },
    async ({ name }) => {
      const exports = targets.getExports();
      const exportNames = Object.keys(exports);
      const matchingExports = exportNames.filter(exportName => exportName.toUpperCase().includes(name.toUpperCase()));
      return {content: [{ type: "text", text: JSON.stringify(matchingExports.map(exportName => exports[exportName])) }]}
    }
  );

  // TODO: export references

  return server;
}