create or replace procedure SHOWEMPS (IN dept char(3)) 
LANGUAGE RPGLE  
EXTERNAL NAME EMPLOYEES GENERAL;