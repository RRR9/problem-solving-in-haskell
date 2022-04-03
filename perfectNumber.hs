perfectNumber :: (Integral a) => a -> Bool
perfectNumber n = sum (divisors n) == n

divisors :: (Integral a) => a -> [a]
divisors n = [x | x <- [1..n-1], n `mod` x == 0]

semiperfectNumber :: (Integral a) => a -> Bool
semiperfectNumber n = isPartialSum n (divisors n)

isPartialSum :: (Integral a) => a -> [a] -> Bool
isPartialSum 0 [] = True
isPartialSum n [] = False
isPartialSum n (h:t)
    | n < 0  = False
    | n == h = True
    | isPartialSum (n-h) t = True
    | isPartialSum n t = True
    | otherwise = False