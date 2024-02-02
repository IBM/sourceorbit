**free

ctl-opt nomain;

dcl-proc doubleIt export;
  dcl-pi *n packed(11:2);
    inputNum packed(11:2) value;
  end-pi;
  
  return inputNum * 2;
end-proc;
