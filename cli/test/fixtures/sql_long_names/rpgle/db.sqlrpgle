**free

ctl-opt nomain;

dcl-proc getLiveBalance;
  dcl-pi *n likeDs(liveResultT);
    type char(1) const;
    cusno int(10) const;
  end-pi;

  dcl-ds liveResult likeds(liveResultT);

  dcl-f trans qualified usropn usage(*input);
  dcl-ds transaction likerec(trans.transfmt);

  liveResult.total = 0;
  liveResult.count = 0;

  select;
    when (type = RLA);
      // This RLA check is a bit of a hack.
      // In the future, we might want to use
      // a keyed field or an LF to look for
      // the card by customer id instead of
      // manually scanning the file.

      // The larger the file, the longer this will take

      OPEN trans;
      
      read trans.transfmt transaction;
      dow not %eof;
        if (transaction.TCUS = cusno);
          liveResult.total += transaction.TAMT;
          liveResult.count += 1;
        endif;
        read trans.transfmt transaction;
      enddo;

      CLOSE trans;
      
    when (type = SQL);
      EXEC SQL
        SELECT count(*), coalesce(sum(TAMT), 0)
        INTO :liveResult.count, :liveResult.total
        FROM TRANSACTION
        WHERE TCUS = :cusno;
  endsl;

  return liveResult;

end-proc;