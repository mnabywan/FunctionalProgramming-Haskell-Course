sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs


f8 = \x -> let y =sqrt x 
            in 2 * y^3 * (y+1)


f9 = \x -> if(x==1) then 1
            else 0

let f10 = \x -> case (x>0) of 
                True -> "Tak"
                False -> "Nie"


sumWith :: Num a=> (a-> a) -> [a] ->a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs

-- sumWith (\x->x^2) [6,7,3,2,3,5]

sumSqr = sumWith (\e -> e^2)
sumAbs = sumWith (\x-> abs(x))
sumPierw = sumWith (\x-> sqrt(x))

lenghtList = sumWith (\x->1)


