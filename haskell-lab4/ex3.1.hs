data BinIntTree = EmptyIntBT | IntNodeBT Int BinIntTree BinIntTree
sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt


data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

depthOfBinTree :: BinTree a -> Int
depthOfBinTree EmptyBT = 0
depthOfBinTree (NodeBT n lt rt) = 1 + max (depthOfBinTree(lt))  (depthOfBinTree(rt))

flattenBT :: BinTree a -> [a]
flattenBT EmptyBT = []
flattenBT (NodeBT n lt rt) = flattenBT lt ++ [n] ++ flattenBT rt 

mapBT :: (a-> b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

elemOf :: Eq a => a-> BinTree a -> Bool
elemOf b EmptyBT = False
elemOf b (NodeBT n lt rt) = if (n == b) then True
                            else elemOf b lt ||  elemOf b rt


                            
treeSize :: BinTree a -> Int 
treeSize EmptyBT = 0
treeSize (NodeBT n lt rt) = 1+ (treeSize lt) + (treeSize rt)

--minElemOf :: (Num a Ord a) => BinTree a -> a
---minElemOf EmptyBT = 0
--minElemOf (NodeBT n lt rt) = min (min (n) (minElemOf lt)) (minElemOf rt)

data Expr a = Lit a | Add (Expr a) (Expr a)
eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
