
        Ctl-Opt DFTACTGRP(*no);

        Dcl-Pr Employees ExtPgm;
          DepartmentNumber Char(3);
        End-Pr;

        Dcl-Pr NewEmp ExtPgm;
          DepartmentNumber Char(3);
        End-Pr;

      //---------------------------------------------------------------*

      /include 'qrpgleref/constants.rpgleinc'



      //---------------------------------------------------------------*

     Fdepts     CF   E             WorkStn Sfile(SFLDta:Rrn)
     F                                     IndDS(WkStnInd)
     F                                     InfDS(fileinfo)

          Dcl-S Exit Ind Inz(*Off);

          Dcl-S Rrn          Zoned(4:0) Inz;

          Dcl-DS WkStnInd;
            ProcessSCF     Ind        Pos(21);
            ReprintScf     Ind        Pos(22);
            Error          Ind        Pos(25);
            PageDown       Ind        Pos(30);
            PageUp         Ind        Pos(31);
            SflEnd         Ind        Pos(40);
            SflBegin       Ind        Pos(41);
            NoRecord       Ind        Pos(60);
            SflDspCtl      Ind        Pos(85);
            SflClr         Ind        Pos(75);
            SflDsp         Ind        Pos(95);
          End-DS;

     DFILEINFO         DS
     D  FILENM           *FILE
     D  CPFID                 46     52
     D  MBRNAM               129    138
     D  FMTNAM               261    270
     D  CURSED               370    371B 0
     D  FUNKEY               369    369
     D  SFLRRN_TOP           378    379B 0
     D  SF_RRN               376    377I 0
     D  SF_RCDS              380    381I 0

      //---------------------------------------------------------------*
      *
          Dcl-S Index Int(5);

          Dcl-Ds Department ExtName('DEPARTMENT') Alias Qualified;
          End-Ds;

          

        //------------------------------------------------------------reb04
          Exit = *Off;
          LoadSubfile();

          Dow (Not Exit);
            Write FOOTER_FMT;
            Exfmt SFLCTL;

            Select;
              When (Funkey = F03);
                Exit = *On;
              When (Funkey = ENTER);
                HandleInputs();
            Endsl;
          Enddo;

          *INLR = *ON;
          Return;

        //------------------------------------------------------------

          Dcl-Proc ClearSubfile;
            SflDspCtl = *Off;
            SflDsp = *Off;

            Write SFLCTL;

            SflDspCtl = *On;

            rrn = 0;
          End-Proc;

          Dcl-Proc LoadSubfile;
            Dcl-S lCount  Int(5);
            Dcl-S Action  Char(1);
            Dcl-S LongAct Char(3);

            ClearSubfile();

            EXEC SQL DECLARE deptCur CURSOR FOR
              SELECT DEPTNO, DEPTNAME
              FROM DEPARTMENT;

            EXEC SQL OPEN deptCur;

            if (sqlstate = '00000');

              dou (sqlstate <> '00000');
                EXEC SQL
                  FETCH NEXT FROM deptCur
                  INTO :Department.DEPTNO, :Department.DEPTNAME;

                if (sqlstate = '00000');
                  XID   = Department.DEPTNO;
                  XNAME = Department.DEPTNAME;

                  rrn += 1;
                  Write SFLDTA;
                endif;
              enddo;

            endif;

            EXEC SQL CLOSE deptCur;

            If (rrn > 0);
              SflDsp = *On;
              SFLRRN = 1;
            Endif;
          End-Proc;

          Dcl-Proc HandleInputs;
            Dcl-S SelVal Char(1);

            Dou (%EOF(depts));
              ReadC SFLDTA;
              If (%EOF(depts));
                Iter;
              Endif;

              SelVal = %Trim(XSEL);

              Select;
                When (SelVal = '5');
                  //DSPLY @XID;
                  Employees(XID);
                When (SelVal = '8');
                  // Insert new employee screen
                  NewEmp(XID);
              Endsl;

              If (XSEL <> *Blank);
                XSEL = *Blank;
                Update SFLDTA;
                SFLRRN = rrn;
              Endif;
            Enddo;
          End-Proc;
