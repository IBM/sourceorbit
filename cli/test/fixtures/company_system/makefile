BIN_LIB=DEV
APP_BNDDIR=APP
LIBL=$(BIN_LIB)

INCDIR=""
BNDDIR=($(BIN_LIB)/$(APP_BNDDIR))
PREPATH=/QSYS.LIB/$(BIN_LIB).LIB
SHELL=/QOpenSys/usr/bin/qsh

all: .logs .evfevent library $(PREPATH)/EMPLOYEES.PGM $(PREPATH)/DEPTS.PGM


.logs:
	mkdir .logs
.evfevent:
	mkdir .evfevent
library:
	-system -q "CRTLIB LIB($(BIN_LIB))"


$(PREPATH)/MYPGM.PGM: qrpglesrc/mypgm.pgm.rpgle
	liblist -c $(BIN_LIB);\
	liblist -a $(LIBL);\
	system "CRTBNDRPG PGM($(BIN_LIB)/MYPGM) SRCSTMF('qrpglesrc/mypgm.pgm.rpgle') OPTION(*EVENTF) DBGVIEW(*SOURCE) TGTRLS(*CURRENT) TGTCCSID(*JOB) BNDDIR($(BNDDIR)) DFTACTGRP(*no)" > .logs/mypgm.splf
	system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/MYPGM.MBR') TOSTMF('.evfevent/mypgm.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"

$(PREPATH)/DEPTS.PGM: qrpglesrc/depts.pgm.sqlrpgle
	system -s "CHGATR OBJ('qrpglesrc/depts.pgm.sqlrpgle') ATR(*CCSID) VALUE(1252)"
	liblist -c $(BIN_LIB);\
	liblist -a $(LIBL);\
	system "CRTSQLRPGI OBJ($(BIN_LIB)/DEPTS) SRCSTMF('qrpglesrc/depts.pgm.sqlrpgle') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) COMPILEOPT('BNDDIR($(BNDDIR)) DFTACTGRP(*no)')" > .logs/depts.splf
	system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/DEPTS.MBR') TOSTMF('.evfevent/depts.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"
$(PREPATH)/EMPLOYEES.PGM: qrpglesrc/employees.pgm.sqlrpgle
	system -s "CHGATR OBJ('qrpglesrc/employees.pgm.sqlrpgle') ATR(*CCSID) VALUE(1252)"
	liblist -c $(BIN_LIB);\
	liblist -a $(LIBL);\
	system "CRTSQLRPGI OBJ($(BIN_LIB)/EMPLOYEES) SRCSTMF('qrpglesrc/employees.pgm.sqlrpgle') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) COMPILEOPT('BNDDIR($(BNDDIR)) DFTACTGRP(*no)')" > .logs/employees.splf
	system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/EMPLOYEES.MBR') TOSTMF('.evfevent/employees.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"


$(PREPATH)/BANKING.MODULE: qrpglesrc/banking.sqlrpgle
	system -s "CHGATR OBJ('qrpglesrc/banking.sqlrpgle') ATR(*CCSID) VALUE(1252)"
	liblist -c $(BIN_LIB);\
	liblist -a $(LIBL);\
	system "CRTSQLRPGI OBJ($(BIN_LIB)/BANKING) SRCSTMF('qrpglesrc/banking.sqlrpgle') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) OBJTYPE(*MODULE)" > .logs/banking.splf
	system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/BANKING.MBR') TOSTMF('.evfevent/banking.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"
$(PREPATH)/UTILS.MODULE: qrpglesrc/utils.sqlrpgle
	system -s "CHGATR OBJ('qrpglesrc/utils.sqlrpgle') ATR(*CCSID) VALUE(1252)"
	liblist -c $(BIN_LIB);\
	liblist -a $(LIBL);\
	system "CRTSQLRPGI OBJ($(BIN_LIB)/UTILS) SRCSTMF('qrpglesrc/utils.sqlrpgle') COMMIT(*NONE) DBGVIEW(*SOURCE) OPTION(*EVENTF) OBJTYPE(*MODULE)" > .logs/utils.splf
	system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/UTILS.MBR') TOSTMF('.evfevent/utils.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"


$(PREPATH)/DEPTS.FILE: qddssrc/depts.dspf
	-system -qi "CRTSRCPF FILE($(BIN_LIB)/qddssrc) RCDLEN(112)"
	system "CPYFRMSTMF FROMSTMF('qddssrc/depts.dspf') TOMBR('$(PREPATH)/qddssrc.FILE/DEPTS.MBR') MBROPT(*REPLACE)"
	liblist -c $(BIN_LIB);\
	liblist -a $(LIBL);\
	system "CRTDSPF FILE($(BIN_LIB)/DEPTS) SRCFILE($(BIN_LIB)/qddssrc) SRCMBR(DEPTS) OPTION(*EVENTF)" > .logs/depts.splf
	system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/DEPTS.MBR') TOSTMF('.evfevent/depts.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"
$(PREPATH)/EMPS.FILE: qddssrc/emps.dspf
	-system -qi "CRTSRCPF FILE($(BIN_LIB)/qddssrc) RCDLEN(112)"
	system "CPYFRMSTMF FROMSTMF('qddssrc/emps.dspf') TOMBR('$(PREPATH)/qddssrc.FILE/EMPS.MBR') MBROPT(*REPLACE)"
	liblist -c $(BIN_LIB);\
	liblist -a $(LIBL);\
	system "CRTDSPF FILE($(BIN_LIB)/EMPS) SRCFILE($(BIN_LIB)/qddssrc) SRCMBR(EMPS) OPTION(*EVENTF)" > .logs/emps.splf
	system "CPYTOSTMF FROMMBR('$(PREPATH)/EVFEVENT.FILE/EMPS.MBR') TOSTMF('.evfevent/emps.evfevent') DBFCCSID(*FILE) STMFCCSID(1208) STMFOPT(*REPLACE)"




$(PREPATH)/DEPARTMENT.FILE: qddssrc/department.table
	liblist -c $(BIN_LIB);\
	liblist -a $(LIBL);\
	system "RUNSQLSTM SRCSTMF('qddssrc/department.table') COMMIT(*NONE)" > .logs/department.splf
$(PREPATH)/EMPLOYEE.FILE: qddssrc/employee.table
	liblist -c $(BIN_LIB);\
	liblist -a $(LIBL);\
	system "RUNSQLSTM SRCSTMF('qddssrc/employee.table') COMMIT(*NONE)" > .logs/employee.splf


$(PREPATH)/BANKING.SRVPGM: qsrvsrc/banking.bnd
	-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$(APP_BNDDIR))"
	liblist -c $(BIN_LIB);\
	liblist -a $(LIBL);\
	system "CRTSRVPGM SRVPGM($(BIN_LIB)/BANKING) MODULE(BANKING) SRCSTMF('qsrvsrc/banking.bnd') BNDDIR($(BNDDIR)) REPLACE(*YES)" > .logs/banking.splf
	-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ((*LIBL/BANKING *SRVPGM *IMMED))"
$(PREPATH)/UTILS.SRVPGM: qsrvsrc/utils.bnd
	-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$(APP_BNDDIR))"
	liblist -c $(BIN_LIB);\
	liblist -a $(LIBL);\
	system "CRTSRVPGM SRVPGM($(BIN_LIB)/UTILS) MODULE(UTILS) SRCSTMF('qsrvsrc/utils.bnd') BNDDIR($(BNDDIR)) REPLACE(*YES)" > .logs/utils.splf
	-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$(APP_BNDDIR)) OBJ((*LIBL/UTILS *SRVPGM *IMMED))"


$(PREPATH)/%.BNDDIR: 
	-system -q "CRTBNDDIR BNDDIR($(BIN_LIB)/$*)"
	-system -q "ADDBNDDIRE BNDDIR($(BIN_LIB)/$*) OBJ($(patsubst %.SRVPGM,(*LIBL/% *SRVPGM *IMMED),$(notdir $^)))"




