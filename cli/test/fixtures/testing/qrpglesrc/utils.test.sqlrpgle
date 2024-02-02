**free

ctl-opt nomain;

/copy 'qrpgleref/utils.rpgleinc'

dcl-proc test_tolower export;
  dcl-s result char(20);
  result = ToLower('HELLO');

  assert(result = 'hello');
end-proc;