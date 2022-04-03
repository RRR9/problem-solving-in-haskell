dividers :: Integer -> [Integer]
dividers n = dividers' n [1] 2

dividers' :: Integer -> [Integer] -> Integer -> [Integer]
dividers' n l d =
    if d * 2 <= n then
        if n `mod` d == 0 then
            dividers' n (d:l) (d + 1)
        else dividers' n l (d + 1)
    else l