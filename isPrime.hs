isPrime :: Integer -> Bool
isPrime n = isPrime' n 3

isPrime' :: Integer -> Integer -> Bool
isPrime' n m =
    if n <= 3 then
        if n == 2 || n == 3 then
            True
        else False
    else if n `mod` m == 0 || n `mod` 2 == 0 then
        False
    else if m * m > n then
        True
    else isPrime' n (m + 2)