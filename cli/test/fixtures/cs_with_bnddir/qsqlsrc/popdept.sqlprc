-------------------------------------------------------------------------------
-- This procedure will create 5 records into the department table
-------------------------------------------------------------------------------

create or replace procedure popdept()
language sql
Result Sets 0
Modifies SQL Data
Specific popdept
begin
    declare i int default 1;
    declare deptno char(3);
    declare deptname varchar(36);
    declare mgrno char(6);
    declare admrdept char(3);
    declare loc char(16);

    while i <= 5 do
        -- Generate random data (you can adjust this as needed)
        set deptno = right('000' || cast(rand()*1000 as int), 3);
        set mgrno = right('00000' || cast(rand()*1000000 as int), 6);
        set admrdept = right('000' || cast(rand()*1000 as int), 3);
        set loc = 'Location ' || deptno;

        -- Assign department names based on specified categories
        case
            when i = 1 then set deptname = 'Admin';
            when i = 2 then set deptname = 'IT';
            when i = 3 then set deptname = 'Finance';
            when i = 4 then set deptname = 'Management';
            when i = 5 then set deptname = 'HR';
        end case;

        -- Insert into department table
        insert into department (deptno, deptname, mgrno, admrdept, location)
        values (deptno, deptname, mgrno, admrdept, loc) with nc;

        set i = i + 1;
    end while;
end;