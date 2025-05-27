**free

ctl-opt nomain;

/copy 'qrpgleref/empdet.rpgleinc'

dcl-proc getEmployeeDetail export;
  dcl-pi *n like(employee_detail_t);
    empno char(6) const;
  end-pi;

  dcl-ds employee_detail likeds(employee_detail_t);

  exec sql
    select
      rtrim(firstnme) || ' ' || rtrim(midinit) || ' ' || rtrim(lastname),
      salary + bonus + comm
    into
      :employee_detail.name,
      :employee_detail.netincome
    from
      employee
    where
      empno = :empno;

  if (sqlcode = 0);
    employee_detail.found = *on;
  else;
    employee_detail.found = *off;
  endif;

  return employee_detail;
end-proc;

dcl-proc getDeptDetail export;
  dcl-pi *n like(department_detail_t);
    deptno char(3) const;
  end-pi;

  dcl-ds department_detail likeds(department_detail_t);

  exec sql
    select
      rtrim(deptname),
      coalesce(location, 'N/A'),
      (select sum(salary + bonus + comm)
       from employee
       where workdept = :deptno)
    into
      :department_detail.deptname,
      :department_detail.location,
      :department_detail.totalsalaries
    from
      department
    where
      deptno = :deptno;

  if (sqlcode = 0);
    department_detail.found = *on;
  else;
    department_detail.found = *off;
  endif;

  return department_detail;
end-proc;