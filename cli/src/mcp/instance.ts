import { McpServer } from "@modelcontextprotocol/sdk/server/mcp";
import { Targets } from "../targets";
import { z } from "zod";

export function createMcpInstance(targets: Targets) {
  const server = new McpServer({
    name: "Source Orbit",
    version: "1.0.0"
  });

  server.tool("add", "Add two numbers", 
    { a: z.number(), b: z.number() },
    async ({ a, b }) => ({
      content: [{ type: "text", text: String(a + b) }]
    })
  );

  return server;
}