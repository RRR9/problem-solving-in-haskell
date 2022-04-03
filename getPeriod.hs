beginning :: Eq a => [a] -> [a]
beginning [] = []
beginning (x:xs) = beginning' (x:[]) xs

beginning' :: Eq a => [a] -> [a] -> [a]
beginning' prd [] = prd
beginning' prd (x:xs)
    | find == True = prd
    | find == False = beginning' (prd ++ x:[]) xs
    where
        find = check prd (x:xs)

check :: Eq a => [a] -> [a] -> Bool
check prd xs = check' prd xs []

check' :: Eq a => [a] -> [a] -> [a] -> Bool
check' [] [] _ = True
check' prd xs zs
    | length prd == 0 = check' zs xs []
check' _ [] _ = False
check' (y:prd) (x:xs) zs
    | y == x = check' prd xs (zs ++ y:[])
    | y /= x = False