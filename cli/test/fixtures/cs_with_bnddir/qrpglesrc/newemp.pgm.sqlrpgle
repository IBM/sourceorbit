**free

ctl-opt dftactgrp(*no);

// TODO: need a way to let the parent program pass in a department id

dcl-pi NEWEMP;
  currentDepartment Char(3);
end-pi;

// ---------------------------------------------------------------*

/INCLUDE 'qrpgleref/constants.rpgleinc'

// ---------------------------------------------------------------*

Dcl-F nemp WORKSTN IndDS(WkStnInd) InfDS(fileinfo);

Dcl-S Exit Ind Inz(*Off);

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

Dcl-DS FILEINFO;
  FUNKEY         Char(1)    Pos(369);
End-DS;

Dcl-Ds Employee ExtName('EMPLOYEE') Alias Qualified;
End-Ds;

Dcl-s autoEmpId char(6);
dcl-s currentError like(XERR);

autoEmpId = getNewEmpId();

if (autoEmpId = '');
  XERR = 'Unable to automatically generate an new ID.';
else;
  XID = autoEmpId;
Endif;

XDEPT = currentDepartment;

Dow (NOT Exit);

  Write HEADER_FMT;
  Exfmt DETAIL;

  currentError = GetError();

  if (FUNKEY = F12);
    Exit = *On;

  elseif (currentError = '');
    // TODO: handle insert and exit
    
    if (HandleInsert());
      Exit = *on;
    else;
      XERR = 'Unable to create employee.';
    endif;

  else;
    XERR = currentError;
  endif;

Enddo;

return;

Dcl-Proc HandleInsert;
  Dcl-Pi *N ind End-Pi;

  Dcl-ds newEmp LikeDS(Employee);

  newEmp.EMPNO = XID;
  newEmp.FIRSTNME = XFIRST;
  newEmp.MIDINIT = XINIT;
  newEmp.LASTNAME = XLAST;
  newEmp.WORKDEPT = currentDepartment;
  newEmp.JOB = XJOB;
  newEmp.HIREDATE = %Date;
  newEmp.PHONENO = XTEL;

  // We don't actually care about these fields.
  newEmp.BIRTHDATE = %Date;
  newEmp.EDLEVEL = 0;
  newEmp.BONUS = 0;
  newEmp.COMM = 0;

  // We can assume it is safe here as we should
  // have already validated it is a number.
  newEmp.SALARY = %dec(XSAL: 9: 2);

  EXEC SQL
    insert into employee
    values (:newEmp)
    with nc;

  return (sqlstate = '00000');
End-Proc;

Dcl-Proc GetError;
  Dcl-Pi *N Like(XERR) End-Pi;
  dcl-s salaryNumber like(Employee.SALARY);
  dcl-s phoneNumber int(5);

  if (XFIRST = '');
    return 'First name cannot be blank';
  endif;

  if (XINIT = '');
    return 'Middle initial cannot be blank';
  endif;

  if (XLAST = '');
    return 'Last name cannot be blank';
  endif;

  // We have left this in so the user
  // cannot continue if no dept is passed in.
  if (XDEPT = '');
    return 'Department cannot be blank';
  endif;

  if (XJOB = '');
    return 'Phone number cannot be blank';
  endif;

  if (XSAL = '');
    return 'Salary cannot be blank';
  else;
    // Validate it is a number
    monitor;
      salaryNumber = %dec(XSAL: 9: 2);
    on-error;
      return 'Salary must be a number';
    endmon;
  endif;

  if (XTEL = '');
    return 'Phone cannot be blank';
  else;
    // Validate it is a number
    monitor;
      phoneNumber = %int(XTEL);
    on-error;
      return 'Phone must be a number';
    endmon;
  endif;

  return '';
End-Proc;

///
// This is needed because empid is not
// auto incremement or auto generating.
// Returns blank if error.
///
Dcl-Proc getNewEmpId;
  Dcl-Pi *N Char(6) End-Pi;

  dcl-s result char(6);
  dcl-s asChar varchar(6);
  dcl-s startI int(5);
  Dcl-s highestEmpId int(10);

  result = '000000';

  EXEC SQL
    select max(int(empno))
    into :highestEmpId
    from employee;
    
  if (sqlstate = '00000');
    asChar = %Char(highestEmpId+100);
    startI = 7 - %len(asChar);
    %subst(result : startI) = asChar;
    Return result;
  endif;

  return '';
End-Proc;