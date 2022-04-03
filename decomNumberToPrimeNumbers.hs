isBNumber :: (Integral a, Ord a) => a -> [a] -> Bool
isBNumber 1 _ = False
isBNumber n xs = isTrue xs (check n 2 1)

check :: (Integral a, Ord a) => a -> a -> a -> [a]
check n d prev
    | n == 1 = []
    | n `mod` d == 0 = 
        if prev /= d
        then d : check (n `div` d) d d
        else check (n `div` d) d prev
    | n `mod` d > 0 = check n (d + 1) prev

isTrue :: (Integral a, Ord a) => [a] -> [a] -> Bool
isTrue _ [] = True
isTrue xs (y:ys)
    | y `elem` xs = isTrue xs ys
    | otherwise = False