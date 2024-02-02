**free

ctl-opt nomain;

/copy 'qrpgleref/utils.rpgleinc'

dcl-pr assert extproc('assert');
  cond int;
end-pr;

dcl-proc test_tolower;
  dcl-s result char(20);
  result = ToLower('HELLO');

  assert(result = 'hello');
end-proc;