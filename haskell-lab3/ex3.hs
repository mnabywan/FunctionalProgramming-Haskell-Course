sqr x = x^2

funcFactory n = case n of
    1 -> id
    2 -> sqr 
    3-> (^3)
    4 -> \x -> x^4
    5-> intFunc
    _->const n
    where intFunc x =x^5
--expApproxUpTo :: Int -> Double -> Double
--expApproxUpTo n x =  sum(x^k/(silnia(k)) where k = [1..n]

silnia 0 =1
silnia n = n* silnia(n-1)

funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x ]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)