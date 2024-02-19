**free

dcl-pr createCustomer int(10) extproc('CREATECUSTOMER');
  type char(1) const;
end-pr;

ctl-opt dftactgrp(*no);

createCustomer('1');
createCustomer('2');

return;