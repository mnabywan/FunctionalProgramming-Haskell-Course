data Cart3DVec a =  MkCart3DVec {x::a, y::a, z::a}
xCoord1 :: Cart3DVec a -> a
xCoord1 (MkCart3DVec {x=xVal, y=_, z=_}) = xVal

data Shape = Circle Float | Rectangle Float Float
area :: Shape -> Float
area (Circle r) = pi*r^2
area (Rectangle a b) = a*b

data TraficLights = Red | Yellow | Green
actionFor :: TraficLights -> String
actionFor Red = "stoj"
actionFor Yellow = "uwaga"
actionFor Green = "Jedz"

