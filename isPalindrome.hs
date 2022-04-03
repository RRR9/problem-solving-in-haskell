palindrom :: [Char] -> Bool
palindrom [a] = True
palindrom [] = True
palindrom xs
    | head xs == last xs = palindrom t
    | otherwise = False
    where
        t = init (tail xs)