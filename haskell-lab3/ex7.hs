onlyEven [] = []
onlyEven (x:xs)
 |(x `mod` 2 == 0) = x : onlyEven xs
 |otherwise       = onlyEven xs

onlyUpper :: (Num a, Ord a) => a -> [a] -> [a]
onlyUpper _ [] = []
onlyUpper n (x:xs) = if (x>n) then x: onlyUpper n xs 
                        else onlyUpper n xs

sumWith :: Num a=> (a-> a) -> [a] ->a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sumAbs = sumWith (\x->abs(x))


sumWith g []     = 0
sumWith g (x:xs) = g x + sumWith g xs

