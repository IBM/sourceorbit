**free

ctl-opt nomain;

dcl-proc ToLower export;
  dcl-pi ToLower char(50);
    inputString char(50) value;
  end-pi;

  exec sql set :inputString = LOWER(:inputString);

  return inputString;
end-proc;