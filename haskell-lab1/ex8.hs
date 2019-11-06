min3Int :: Int -> Int -> Int -> Int
min3Int a b c | a<b && a<c = a
	      | b<c =b 
	      | otherwise = c
