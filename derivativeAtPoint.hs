polyValue :: [Integer] -> Integer -> Integer
polyValue list x = polyValue' list x 0

polyValue' :: [Integer] -> Integer -> Integer -> Integer
polyValue' (h:t) x acc 
    | length (h:t) > 2 = polyValue' t x (acc + h * toInteger(length t) * x ^ (toInteger(length t) - 1))
    | otherwise = acc + h