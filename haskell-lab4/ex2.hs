
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y


data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y


data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

data List a = EmptyL | Cons a (List a) deriving Show
head' :: List a -> a
head' EmptyL = error "Pusta lista"
head' (Cons x xs) = x

data ThreeColors = Blue | White | Red
type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"


data Person = Person String String deriving (Show)
name :: Person -> String
name (Person n _) = n

data Person' = Person' {
    name' :: String,
    surname' :: String,
    age' :: Int
}

data Shape = Circle Float
            | Rectangle Float Float
area :: Shape -> Float
area (Circle r) = pi*r^2
area (Rectangle a b) = a*b

data Tree a = Nil |
            Node a (Tree a) (Tree a)
sumTree :: Tree Int-> Int
sumTree Nill = 0
sumTree (Node n lt rt) = n+ sumTree lt + sumTree rt

TreeToList :: Tree a -> [a]
TreeToList Nill = []
TreeToList (Node n lt rt) = TreeToList lt ++ [n] ++ TreeToList rt

collapse :: Tree a -> [a]
collapse Nill = []
collapse (Node n lt rt) = collapse lt ++ [n] ++ collapse rt

