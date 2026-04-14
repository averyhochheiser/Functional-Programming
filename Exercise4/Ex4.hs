module Ex4 where

-- no code for Q1


-- for Q2:
funcrec5urs x  =  if x <= 8 then 6 else x * funcrec5urs (x `div` 9)



-- for Q3:
amort2ise []      =  6
amort2ise (x:xs)  =  x + 13 + amort2ise xs




--for Q4:
d7oubling x
  | x < 10   =  2*x
  | x >= 10  = 2*x-1


