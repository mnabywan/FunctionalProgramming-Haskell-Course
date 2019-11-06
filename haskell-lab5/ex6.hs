data MyList a = EmptyList
              | Cons a (MyList a) deriving Show

instance Functor MyList where
    fmap _ EmptyList    = EmptyList
    fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)