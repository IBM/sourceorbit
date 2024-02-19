**free

ctl-opt nomain;

dcl-pr getFirst char(15) extproc('GETFIRST');
end-pr;

dcl-pr getSurname char(15) extproc('GETSURNAME');
end-pr;

dcl-pr random int(10) extproc('RANDOM');
  low packed(7) const;
  high packed(7) const;
end-pr;

dcl-pr getStreetType char(3) extproc('GETSTREETTYPE');
end-pr;

dcl-pr getCity char(15) extproc('GETCITY');
end-pr;

dcl-pr getState char(2) extproc('GETSTATE');
end-pr;

dcl-proc createCustomer export;
  dcl-pi *n int(10);
    type char(1) const;
  end-pi;

  dcl-f customer qualified keyed usropn usage(*output);
  dcl-ds cust likerec(customer.custfmt);

  cust.FIRSTNME = getFirst();
  cust.LASTNAME = getSurname();
  cust.PHONENO = %char(random(1111:9999));
  cust.CREATED = %date;
  cust.ADDR1 = %char(random(1:9999)) + ' ' + %trimr(getSurname()) + ' ' + getStreetType();
  cust.CITY = getCity();
  cust.STATE = getState();
  cust.ZIP = %char(random(10000:99999));

  select;
    when (type = '1'); //RLA
      OPEN customer;
      write customer.CUSTFMT cust;
      CLOSE customer;
    when (type = '2'); //SQL
      EXEC SQL
        INSERT INTO CUSTOMER
        VALUES(:cust);
      
    other;
      return -1;
  endsl;

end-proc;