**FREE

exec sql declare c1 cursor for 
        with temp as (
          select
           field1,
           field2
          from table1
        )
Select * from temp;