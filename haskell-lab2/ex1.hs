myFun x = 2 * x


add2T :: Num a=> (a,a) -> a
add2T (a,b) = a+b

add2C :: Num a  =>(a->a)->a
add2C x y =x+ y


add3C :: Num a => a-> a-> a-> a
add3C a b c = a+b+c

add3T :: Num a => (a, a, a) ->a
add3T (a,b,c) = a+b+c

fivetoPower_ :: Intiger -> Intiger
fivetoPower_ = 2