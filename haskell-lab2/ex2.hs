
fiveToPower a = (5^) a

_ToPower5 :: Num a => a -> a
_ToPower5 n = ( ^ 5) n

isPalindrome :: [Char] -> Bool
isPalindrome a | (reverse a) == a = True
               | otherwise = False

prod' :: Num a => [a]->a
prod' [] = 1
prod' (x:xs) = (x*prod' xs)

prod2' :: Num a=> [a]->a
prod2' = loop 1 
    where loop acc [] = acc
          loop acc (x:xs) = loop (x*acc) xs






doubleAll :: Num a => [a]->[a]
doubleAll [] = []
doubleAll (x:xs) = [2*x] ++ doubleAll(xs)


selectEven :: (Num a, Integral a) => [a] -> [a] 
selectEven  = loop [] 
        where loop acc [] = acc 
              loop acc (x:xs) = if (x `mod` 2 == 0) then loop ([x] ++ acc) xs
                                else loop acc xs 































dlugosc :: Num a => [a]->a
dlugosc xs= loop 0 xs
        where loop acc [] = acc
              loop acc (x:xs) = loop (1+acc) xs