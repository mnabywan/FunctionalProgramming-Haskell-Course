actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >> putChar '\n'
doActSeq = do
    putChar 'A'
    putChar 'G'
    putChar 'H'
    putChar '\n'


echo1 = getLine >>= putStrLn
doEcho1 = do
    line <- getLine
    putStrLn line

echo2 = getLine >>= \line -> putStrLn $ line++ "!"

doEcho2 = do
    line <- getLine
    putStrLn $ line ++ "!"

echo3 :: IO ()
echo3 = getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1++ " " ++ l2 ++ "!"



dialog :: IO ()
dialog = 
    putStrLn "What's your happy number?"
    >> getLine 
    >>= \n -> let num = read n :: Int in 
    if num ==7
    then putStrLn "Ah lucky 7!"
    else if odd num 
        then putStrLn "odd number! That;s most people choice!"
        else putStrLn "Hm, even number? Interesting.."

--zad8.1
doEcho3 = do
    l1<- getLine
    putChar '\n'
    l2<- getLine
    putStrLn $ l1 ++" "++ l2 ++ "!!"


--doDialog = do
  --  putStrLn "Whats your lucky number?"
    --l1 <- getChar
    --let num = read l1 :: Int in
    --if (num == 7) 
   -- then putStrLn "Lucky 7 ;)"
   -- else if odd num
     --   then "Odd number, so popular"
    --else "Even num , interesting.."


twoQuestions :: IO ()
twoQuestions = do 
    putStrLn "whats yourname"
    name <- getLine 
    putStrLn "howoldareyou"
    age <- getLine
    print (name,age)

twoQuestions2 = putStrLn "Whatsyourname"
                >> getLine >>= \line1 ->
               putStrLn "Howoldareyou"
               >> getLine  >>= \line2 ->
                print (line1,line2)


--fun = do
    --putStrLn "Podaj imie: "
    --s <- getLine
  --  putStrln $ "Witaj " ++ s

fun2 = putStrLn "Podaj imie: " >> getLine >>= \imie -> putStrLn $ "Witaj" ++ " "  ++ imie ++ "!!!!!!"

doFun = do
    putStrLn "Podaj imie"
    imie <- getLine
    putStrLn $ "Witaj " ++ " " ++ imie ++ "!!!!"

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
    fmap f (Leaf x)            = Leaf   (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

