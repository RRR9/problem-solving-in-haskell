isBinaryHeap :: (Integral a) => [a] -> Bool
isBinaryHeap xs = isBinaryHeap' xs 0

isBinaryHeap' :: (Integral a) => [a] -> Int -> Bool
isBinaryHeap' xs k
    | kl > xsLen = True
    | kl <= xsLen && kr > xsLen && xkl <= xk = True
    | kl <= xsLen && kr <= xsLen && xkl <= xk && xkr <= xk = isBinaryHeap' xs kl && isBinaryHeap' xs kr
    | otherwise = False
    where
        xsLen = (length xs) - 1
        kl = k * 2 + 1
        kr = k * 2 + 2
        xkl = xs !! kl
        xkr = xs !! kr
        xk = xs !! k