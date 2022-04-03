largestDivisible :: Int
largestDivisible = head (filter f [100000, 99999 .. 0])

f :: Int -> Bool
f x = x `mod` 3829 == 0