--https://www.ibm.com/docs/en/i/7.3?topic=tables-department-table-department

CREATE OR REPLACE TABLE DEPARTMENT
      (DEPTNO    CHAR(3)           NOT NULL,
       DEPTNAME  VARCHAR(36)       NOT NULL,
       MGRNO     CHAR(6)           NOT NULL,
       ADMRDEPT  CHAR(3)           NOT NULL, 
       LOCATION  CHAR(16)          NOT NULL,
       PRIMARY KEY (DEPTNO));

ALTER TABLE DEPARTMENT
      ADD FOREIGN KEY ROD (ADMRDEPT)
          REFERENCES DEPARTMENT
          ON DELETE CASCADE;

-- Remove circular reference
--ALTER TABLE DEPARTMENT
--      ADD FOREIGN KEY RDE (MGRNO)
--          REFERENCES EMPLOYEE
--          ON DELETE SET NULL;

-- CREATE UNIQUE INDEX XDEPT1
--        ON DEPARTMENT (DEPTNO);

-- CREATE INDEX XDEPT2
--        ON DEPARTMENT (MGRNO);

-- CREATE INDEX XDEPT3
--        ON DEPARTMENT (ADMRDEPT); 