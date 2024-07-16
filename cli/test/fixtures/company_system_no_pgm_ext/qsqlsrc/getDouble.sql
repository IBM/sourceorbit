Create function getDouble (inputNumber decimal(11, 2))
       returns decimal(11, 2)
       language rpgle
       external name BANKING(doubleIt)
       parameter style general;