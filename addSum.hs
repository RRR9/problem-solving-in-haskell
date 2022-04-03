sumList :: Num a => [a] -> [a]
sumList [] = []
sumList xs = sumList' xs 0

sumList' :: Num a => [a] -> a -> [a]
sumList' [] _ = []
sumList' (x:xs) sum = t : sumList' xs t
    where
        t = x + sum