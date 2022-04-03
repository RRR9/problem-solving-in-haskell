import Data.Char

binToInt :: [Char] -> Int
binToInt [] = 0
binToInt xs = binToInt' xs 0

binToInt' :: [Char] -> Int -> Int
binToInt' [] _ = 0
binToInt' xs p = (ord (last xs) - 48) * 2 ^ p + binToInt' (init xs) (p + 1)