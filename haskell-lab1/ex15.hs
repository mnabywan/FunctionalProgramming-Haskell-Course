vec :: (Double, Double) -> (Double, Double)
vec (a,b)= (a/d, b/d)
           where d=sqrt(a*a + b*b)


vec2 :: (Double, Double) -> (Double, Double)
vec2 (a,b) = let d=sqrt(a*a+ b*b)
            in (a/d, b/d)

signum :: Int -> Int
signum n | n > 0 = 1
         | n == 0 = 0
         | otherwise = -1
