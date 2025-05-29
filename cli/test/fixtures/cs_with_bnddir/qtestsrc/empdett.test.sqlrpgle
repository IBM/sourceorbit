**free

ctl-opt nomain BNDDIR('APP');

/include 'qrpgleref/empdet.rpgleinc'
/include qinclude,TESTCASE

exec sql
  set option commit = *none;

dcl-proc setUpSuite export;
  // Insert sample data into employee
  exec sql
    insert into employee (
      empno, firstnme, midinit, lastname, workdept, phoneno,
      hiredate, job, edlevel, sex, birthdate, salary, bonus, comm
    ) values 
      ('000010', 'CHRISTINE', 'I', 'HAAS', 'A00', '3978', '01/01/65', 'PRES', 18, 'F', null, 52750, 1000, 4220),
      ('000020', 'MICHAEL', 'L', 'THOMPSON', 'B01', '3476', '10/10/73', 'MANAGER', 18, 'M', '02/02/48', 41250, 800, 3300),
      ('200120', 'GREG', '', 'ORLANDO', 'A00', '2167', '05/05/72', 'CLERK', 14, 'M', '10/18/42', 29250, 600, 2340);
  
  if (sqlcode <> 0 and sqlcode <> -803);
    fail('Failed to insert into employee table with SQL code: ' + %char(sqlcode));
  endif;

  // Insert sample data in department table
  exec sql
    insert into department (
      deptno, deptname, mgrno, admrdept, location
    ) values
      ('A00', 'SPIFFY COMPUTER SERVICE DIV.', '000010', 'A00', 'NEW YORK'),
      ('B01', 'PLANNING', '000020', 'A00', 'ATLANTA');

  if (sqlcode <> 0 and sqlcode <> -803);
    fail('Failed to insert into department table with SQL code: ' + %char(sqlcode));
  endif;
end-proc;

dcl-proc test_getEmployeeDetail_found export;
  dcl-pi *n extproc(*dclcase) end-pi;

  dcl-s empno char(6);
  dcl-ds actual likeDS(employee_detail_t);
  dcl-ds expected likeDS(employee_detail_t);

  // Input
  empno = '000010';

  // Actual results
  actual = getEmployeeDetail(empno);

  // Expected results
  expected.found = *on;
  expected.name = 'CHRISTINE I HAAS';
  expected.netincome = 57970;

  // Assertions
  nEqual(expected.found : actual.found : 'found');
  aEqual(expected.name : actual.name : 'name');
  assert(expected.netincome = actual.netincome : 'netincome' );
end-proc;

dcl-proc test_getDeptDetail_found export;
  dcl-pi *n extproc(*dclcase) end-pi;

  dcl-s deptno char(3);
  dcl-ds actual likeDS(department_detail_t);
  dcl-ds expected likeDS(department_detail_t);

  // Input
  deptno = 'A00';

  // Actual results
  actual = getDeptDetail(deptno);

  // Expected results
  expected.found = *on;
  expected.deptname = 'SPIFFY COMPUTER SERVICE DIV.';
  expected.location = 'NEW YORK';
  expected.totalsalaries = 90160;

  // Assertions
  nEqual(expected.found : actual.found : 'found');
  aEqual(expected.deptname : actual.deptname : 'deptname');
  aEqual(expected.location : actual.location : 'location');
  assert(expected.totalsalaries = actual.totalsalaries : 'totalsalaries');
end-proc;