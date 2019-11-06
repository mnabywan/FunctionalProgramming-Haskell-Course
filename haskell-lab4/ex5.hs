data MyInt = MyInt Int
instance Eq MyInt where
    (==) (MyInt i1) (MyInt i2) = i1 == i2
instance Ord MyInt where 
    (<=) (MyInt i1) (MyInt i2) = i1 <= i2

instance Num MyInt where
    (+) (MyInt i1) (MyInt i2) = MyInt(i1 + i2)
    (-) (MyInt i1) (MyInt i2) = MyInt (i1 - i2)
    (*) (MyInt i1) (MyInt i2) = MyInt (i1 * i2)
    negate (MyInt i)            = MyInt (negate i)
    abs (MyInt i)               = MyInt (abs i)
    signum (MyInt i)            = MyInt (signum i)
    fromInteger int               = MyInt (fromIntegral int)

instance Show MyInt where
    show (MyInt i) = "MyInt " ++ show i


data BinTree a = NodeBT (BinTree a) (BinTree a) | Leaf a deriving (Show) 
mapBinTree  :: (a->b) -> BinTree a -> BinTree b

--maptree f (Leaf a)= Leaf (f a) 
--maptree f (NodeBT xl xr ) = NodeBT (maptree f xl) (maptree f xr)
mapBinTree f (Leaf a) = Leaf (f a) 
mapBinTree  f (NodeBT lt rt) = NodeBT (mapBinTree f lt) (mapBinTree f rt)


data MyType = C1 (Bool, Int) | C2 Int | C3 Double
instance Eq MyType where 
     C1 (b1,i1) == C1 (b2, i2) = (b1==b2) && (i1==i2) 
     C2  i == C2 j      =  i==j
     C3 d1 == C3 d2       = (d1==d2)
   --  C2 i == C3 j = (i==j)
     _ == _  = False


data Shape = Circle Float | Rectangle Float Float
instance Eq Shape where
    Circle a1 == Circle a2 = a1==a2
    Rectangle a b == Rectangle c d = a==c && b==d