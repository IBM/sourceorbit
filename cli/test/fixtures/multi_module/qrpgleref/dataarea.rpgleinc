**FREE

// Returned Data Structure
Dcl-DS DSResult_T  Qualified Template;
  BytesAvail     Int(10);
  BytesReturned  Int(10);
  DataType       Char(10);
  Library        Char(10);
  RtnLength      Int(10);
  DecPos         Int(10);
  Data           Char(300);
End-DS;

// Retrieve Data Area API
Dcl-PR getDataArea  ExtPgm('QWCRDTAA');
  ReturnVar      LikeDS(DSResult_T);
  ReturnLen      Int(10)    Const;
  QualDataArea   Char(20)   Const;
  StartPos       Int(10)    Const;
  DataLen        Int(10)    Const;
  ErrorCode      Char(300)  Options(*VarSize);
End-PR;

Dcl-Ds Error_T Qualified Template;
  Content Char(300);
  Code Char(7) Pos(9);
End-Ds;