f3:: Int  -> Int 
f3 n = if n==1 
       then 3
	else if n==2
         then 10
	  else 1

absInt :: Int -> Int
absInt n = case (n>0)of
	  True -> n
	  False -> -n 
          
