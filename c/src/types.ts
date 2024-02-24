export enum BlockType {
  Body = "body",
  List = "list",
  Index = "index",
  Unknown = "unknown",
}

export interface IRange {
  start: number;
  end: number;
}

export interface Token {
  value?: string;
  block?: Token[];
  type: string;
  blockType?: BlockType;
  range: IRange;
}