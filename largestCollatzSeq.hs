maxCollatz :: (Integral a, Ord a) => [a] -> [(Int, a)]
maxCollatz [] = [(0, 0)]
maxCollatz xs = maxCollatz' xs

maxCollatz' :: (Integral a, Ord a) => [a] -> [(Int, a)]
maxCollatz' [] = []
maxCollatz' (x:xs) = (collatz x, x) : maxCollatz' xs

collatz :: (Integral a, Ord a) => a -> Int
collatz x = collatz' x 0

collatz' :: (Integral a, Ord a) => a -> Int -> Int
collatz' 1 count = count + 1
collatz' x count
    | even x = collatz' (x `div` 2) (count + 1)
    | odd x = collatz' (x * 3 + 1) (count + 1)