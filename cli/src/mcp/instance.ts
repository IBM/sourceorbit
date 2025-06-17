import { McpServer } from "@modelcontextprotocol/sdk/server/mcp";
import { ILEObject, ILEObjectTarget, ObjectType, Targets } from "../targets";
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
        targets.getResolvedObjects().filter(obj => obj.systemName.toUpperCase() === name.toUpperCase() || (obj.longName && obj.longName.toUpperCase() === name.toUpperCase()))
      )}]
    })
  );

  server.tool(`findObjectByRelativePath`, "Find object by relative path",
    { relativePath: z.string() },
    async ({ relativePath }) => {
      const fullPath = targets.getRelative(relativePath);
      const object = await targets.resolvePathToObject(fullPath);
      if (!object) {
        return { content: [{ type: "text", text: `No object found at ${fullPath}` }] };
      }

      return { content: [{ type: "text", text: JSON.stringify(object, null, 2) }] };
    }
  );

  server.tool("findObjectsByType", "Find objects by type and name", 
    { name: z.string().optional(), type: OBJECT_TYPES },
    async ({ name, type }) => {
      const objects = targets.getResolvedObjects(type);

      if (name) {
        name = name.trim().toUpperCase()
        return {
          content: [{ type: "text", text: JSON.stringify(
            objects.filter(obj => obj.systemName.toUpperCase().includes(name) || (obj.longName && obj.longName.toUpperCase().includes(name)))
          )}]
        }
      } else {
        return {
          content: [{ type: "text", text: JSON.stringify(objects) }]
        }
      }
    }
  );

  server.tool(`impactsByObject`, "Find objects that are impacted by a given object if changed",
    { name: z.string(), type: OBJECT_TYPES },
    async ({ name, type }) => {
      const impacts = targets.getImpactFor({systemName: name, type: type});
      
      return { content: [{ type: "text", text: JSON.stringify(impacts, null, 2) }] };
    }
  );

  server.tool(`impactsByUri`, "Find objects that are impacted by a given URI if changed",
    { relativePath: z.string() },
    async ({ relativePath }) => {
      const fullPath = targets.getRelative(relativePath);
      const object = await targets.resolvePathToObject(fullPath);
      const impacts = targets.getImpactFor(object);
      
      return { content: [{ type: "text", text: JSON.stringify(impacts, null, 2) }] };
    }
  );

  server.tool(`dataAccessByObject`, "Find what data is used by a given object",
    { name: z.string(), type: OBJECT_TYPES },
    async ({ name, type }) => {
      const object = targets.getTarget({ systemName: name, type: type });

      if (!object) {
        return { content: [{ type: "text", text: `No object found with name ${name} and type ${type}` }] };
      }

      let scanned: ILEObject[] = [];
      let dataObjects: ILEObject[] = [];

      const dataObjectTypes: ObjectType[] = [`FILE`, `DTAARA`, `DTAQ`];

      function collectDataAccess(currentObject: ILEObjectTarget) {
        if (scanned.some(o => o.systemName === currentObject.systemName && o.type === currentObject.type)) {
          return;
        }

        scanned.push(currentObject);

        for (const dep of currentObject.deps) {
          if (dataObjectTypes.includes(dep.type)) {
            if (!dataObjects.some(o => o.systemName === dep.systemName && o.type === dep.type)) {
              dataObjects.push(dep);
            }
          }

          collectDataAccess(targets.getTarget(dep));
        }
      }

      collectDataAccess(object);
      
      return { content: [{ type: "text", text: JSON.stringify(dataObjects, null, 2) }] };
    }
  );

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

  server.tool(`exportImports`, `Find where a given export function/procedure is imported/used`,
    { name: z.string() },
    async ({ name }) => {
      const exports = targets.getExports();
      const exportNames = Object.keys(exports);
      const matchingExport = exportNames.find(exportName => exportName.toUpperCase().includes(name.toUpperCase()));
      const definedIn = exports[matchingExport];

      if (!definedIn) {
        return { content: [{ type: "text", text: `No export found for ${name}` }] };
      }

      const importedBy = targets.getResolvedObjects().filter(o => o.imports?.includes(matchingExport));

      const data = {
        definedIn,
        importedBy
      }

      return { content: [{ type: "text", text: JSON.stringify(data, null, 2) }] };
    }
  );

  server.tool("whereHeaderIsUsed", "Find which objects use this header file/copybook", 
    { relativePath: z.string() },
    async ({ relativePath }) => {
      const objectsUsingHeader = targets
        .getResolvedObjects()
        .filter(o => o.headers?.includes(relativePath))
        .map(o => ({name: o.systemName, type: o.type, relativePath: o.relativePath}));
      
      return { content: [{ type: "text", text: JSON.stringify(objectsUsingHeader, null, 2) }] };
    }
  )

  // TODO: export references

  return server;
}