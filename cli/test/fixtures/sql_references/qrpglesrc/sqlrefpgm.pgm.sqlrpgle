**FREE
Ctl-Opt Main(SQLREFPGM) dftactgrp(*no) option(*srcstmt);

dcl-pr SQLREFPGM extpgm('SQLREFPGM');
end-pr;


Dcl-Proc SQLREFPGM;
  Dcl-Pi *N;
  End-Pi;
  DCL-S CNT packed(5);
  EXEC SQL SELECT COUNT(1) INTO :CNT FROM STOCK;
  DSPLY %CHAR('STOCK: ' + %CHAR(CNT));
  RETURN;
End-Proc;