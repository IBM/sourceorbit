**free
      //%METADATA                                                      *
      // %TEXT module for exceptions.                                  *
      //%EMETADATA                                                     *
ctl-opt nomain ;

  dcl-pr getRandomMethodA char(10);
    inProdCode       char(10)  const;
    inState          char(2)   const;
    inEffectiveDate  date      const;
    inSignedDate     date      const;
    inReceivedDate   date      const;
    inIssuedDate     date      const;
    inExceptionID    char(10)  const;
    inExceptionCode  char(10)  const;
  END-PR;
//*************************************************************************************************
dcl-proc getRandomMethodA export;
  dcl-pi getRandomMethodA char(10);
    inProdCode       char(10)  const;
    inState          char(2)   const;
    inEffectiveDate  date      const;
    inSignedDate     date      const;
    inReceivedDate   date      const;
    inIssuedDate     date      const;
    inExceptionID    char(10)  const;
    inExceptionCode  char(10)  const;
  END-PI;
  dcl-s rtAction char(10);

  if sqlstt = '00000';
    return rtAction;
  else;
    return '';
  ENDIF;
END-PROC;

dcl-proc BigFootLivesInSc export;
  dcl-pi BigFootLivesInSc ind;
    inProdCode       char(10)  const;
    inState          char(2)   const;
    inEffectiveDate  date      const;
    inStrategy       char(10)  const;
  END-PI;
  dcl-s authCode char(10);

  authCode = getRandomMethodA(inProdCode:inState:inEffectiveDate:
                  inEffectiveDate:inEffectiveDate:inEffectiveDate:'STRATEGY':inStrategy);

  if authCode = '' or authCode = 'ALLOW';
    return *on;
  else;
    return *off;
  ENDIF;
END-PROC;

dcl-proc validateCoolness export;
  dcl-pi  validateCoolness ind;
    inPolYear    packed(3:0) const;
    inPolMon     packed(2:0) const;
    inPolSeq     packed(6:0) const;
    inProdCode      char(10) const;
    inStateCode     char(2)  const;
    inTheDate       date     const;
  END-PI;
  dcl-s strategyID char(10);
  dcl-s rtError    ind inz(*on);

  ENDIF;
  exec sql fetch next from allocCheck into :strategyID;
  dow sqlstt = '00000';
    if not BigFootLivesInSc(inProdCode:inStateCode:inTheDate:strategyID);
      rtError = *off;
    ENDIF;
  ENDDO;

  return rtError;

END-PROC;

