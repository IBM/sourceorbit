-- https://www.ibm.com/docs/en/i/7.3?topic=tables-employee-table-employee

CREATE OR REPLACE TABLE EMPLOYEE
      (EMPNO       CHAR(6)         NOT NULL,
       FIRSTNME    VARCHAR(12)     NOT NULL,
       MIDINIT     CHAR(1)         NOT NULL,
       LASTNAME    VARCHAR(15)     NOT NULL,
       WORKDEPT    CHAR(3)                 ,
       PHONENO     CHAR(4)                 ,
       HIREDATE    DATE                    ,
       JOB         CHAR(8)                 ,
       EDLEVEL     SMALLINT        NOT NULL,
       SEX         CHAR(1)                 ,
       BIRTHDATE   DATE                    ,
       SALARY      DECIMAL(9,2)            ,
       BONUS       DECIMAL(9,2)            ,
       COMM        DECIMAL(9,2)            ,    
       PRIMARY KEY (EMPNO)); 

-- Remove circular reference
-- ALTER TABLE EMPLOYEE 
--       ADD FOREIGN KEY RED (WORKDEPT) 
--       REFERENCES DEPARTMENT 
--       ON DELETE SET NULL;

ALTER TABLE EMPLOYEE 
      ADD CONSTRAINT NUMBER 
      CHECK (PHONENO >= '0000' AND PHONENO <= '9998');

-- CREATE UNIQUE INDEX XEMP1 
--        ON EMPLOYEE (EMPNO);

-- CREATE INDEX XEMP2 
--        ON EMPLOYEE (WORKDEPT); 