create or replace function getTotalSalary () 
  returns decimal(9, 2)
  specific GETTOTSAL
begin
  declare total decimal(9, 2);

  select sum(salary) into total from employee;

  return total;
end;