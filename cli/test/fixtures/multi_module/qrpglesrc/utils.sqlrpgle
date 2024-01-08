**FREE

Ctl-Opt NoMain;

/copy 'qrpgleref/system.rpgleinc'
/copy 'qrpgleref/utils.rpgleinc'

Dcl-S JobTypeGlobal Char(1);

Dcl-Proc Utils_Lower Export;
  Dcl-Pi *N Char(10);
    pValue Char(10) Value;
  End-Pi;
          
  EXEC SQL SET :pValue = LOWER(:pValue);
          
  Return pValue;
End-Proc;

Dcl-Proc Utils_Print Export;
  Dcl-Pi *N;
    Text Varchar(512) Const;
  End-Pi;

  Dcl-s Type Char(1);

  Type = Utils_JobType();

  If (Type = 'I');
    printf_jl(%trim(Text) + x'25');
  Else;
    printf(%trim(Text) + x'25');
  Endif;
End-Proc;

Dcl-Proc Utils_Qsh Export;
  Dcl-Pi *N Ind;
    Command Varchar(512) Const;
  End-Pi;

  return system('QSH CMD(''' + %Trim(Command) + ''')') = 0;
End-Proc;

dcl-proc Utils_JobType export;
  dcl-pi *n char(1) end-pi;
  dcl-pr QUSRJOBI extpgm;
    *n char(32766) options(*varsize);
    *n int(10:0) const;
    *n char(8) const;
    *n char(26) const;
    *n char(16) const;
  end-pr;

  dcl-ds Returned len(86);
    JobType char(1) pos(61);
  end-ds;

  If (JobTypeGlobal = *BLANK);
    QUSRJOBI(Returned:%size(Returned):'JOBI0100':'*':'');

    JobTypeGlobal = JobType;
  Endif;
  
  Return JobTypeGlobal;
end-proc;