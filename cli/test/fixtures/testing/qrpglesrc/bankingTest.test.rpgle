**free

ctl-opt nomain;

/copy 'qrpgleref/banking.rpgleinc'

dcl-pr assert extproc('assert');
  cond int;
end-pr;

dcl-proc test_doubleIt export;
  // really does nothing!!
  dsply 'Hello world'
end-proc;