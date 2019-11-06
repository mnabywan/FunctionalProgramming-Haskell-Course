or' :: (Bool, Bool) -> Bool
or' (True, False) = True
or' (False, True) = True
or' (True, True) = True
or' (False,False) = False

max' :: (Int,Int) -> Int
max' (a,b) | a>b = a
	   | otherwise = b


and' :: (Bool, Bool) -> Bool
and' (a,b) = case (a==b && a==True) of
             True->True
	     False ->False 
