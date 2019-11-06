fib :: (Num a, Eq a) => a->a
fib n = 
    if n==0 || n==1 then n
    else fib(n-2)+fib(n-1)


sum' :: Num a => [a]->a
sum' [] = 0
sum' (x:xs) = x+ sum' xs

lenght' :: Num a => [a] -> a
lenght' [] = 0
lenght' (x:xs) = 1+lenght' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = if (x==True) then True else or' xs
  

elem' :: Eq a => a-> [a]->Bool
elem' _ [] = False
elem' a (x:xs) = if (x==a) then True else elem' a xs
                                
squareAll :: Num a => [a] -> [a]
squareAll [] = []
squareAll (x:xs) = [x^2] ++ squareAll xs

selectEven :: Integral a => [a]->[a]
selectEven [] = []
selectEven (x:xs) = if (x `mod` 2 ==0) then [x] ++ selectEven xs
                    else selectEven xs


sum'2 :: Num a => [a]-> a
sum'2 xs = loop 0 xs
    where loop acc [] =acc
          loop acc (x:xs) = loop(x+acc) xs


prod'2 :: Num a => [a]->a
prod'2 xs = loop 1 xs
    where loop acc [] =acc
          loop acc (x:xs) = loop(x*acc) xs


lenght2' :: Num a => [a] -> a
lenght2' xs = loop 0 xs
    where loop acc [] = acc
          loop acc (x:xs) = loop(1+acc) xs      
          
sum'3 :: Num a => [a] -> a
sum'3 = loop 0
      where loop acc []   = acc
            loop acc (x:xs) = loop (x + acc) xs












