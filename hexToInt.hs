idx' :: Eq a => a->[a]->Int
idx' x [] = (-1)
idx' x (h:t)
    | x == h       = 0
    | i < 0 = (-1)
    | otherwise    = 1 + i
    where i = idx' x t

mySignum :: (Num a, Ord a) => a->Int
mySignum x
    | x > 0 = 1
    | x < 0 = (-1)
    | otherwise = 0

hexDigit::Char->Int
hexDigit '0' = 0
hexDigit '1' = 1
hexDigit '2' = 2
hexDigit '3' = 3
hexDigit '4' = 4
hexDigit '5' = 5
hexDigit '6' = 6
hexDigit '7' = 7
hexDigit '8' = 8 
hexDigit '9' = 9
hexDigit 'a' = 10
hexDigit 'b' = 11
hexDigit 'c' = 12
hexDigit 'd' = 13
hexDigit 'e' = 14
hexDigit 'f' = 15
hexDigit x = (-1)

myLength :: [a]->Int
myLength [] = 0
myLength (_:t) = 1 + myLength t

mySum :: Num a => [a]->a
mySum [] = 0
mySum (h:t) = h + mySum t

index :: Char->String->Int
index x y =
    if y == []
    then (-1)
    else if x == head y
         then 0 
         else if index x (tail y) < 0
              then (-1)
              else 1 + index x (tail y)

hexToInt :: String->Int
hexToInt x =
    if x == []
    then 0
    else index (head x) "0123456789abcdef" *
         16^(length (tail x)) + hexToInt (tail x)

hexToIntAcc :: Int->String->Int
hexToIntAcc a x = 
    if x == []
    then a
    else hexToIntAcc (a*16 + index (head x) "0123456789abcdef")
         (tail x)
         
hexToInt' :: String->Int
hexToInt' x = 
    hexToIntAcc 0 x

idx :: Char->String->Int
idx x [] = (-1)
idx x (h:y) =
    if x == h
    then 0
    else
        if idx x y < 0
        then (-1)
        else 1 + idx x y


idx'' :: Char->String->Int
idx'' x [] = (-1)
idx'' x (h:y)
    | x == h      = 0
    | i < 0       = (-1)
    | otherwise   = 1 + i
    where i = idx'' x y
