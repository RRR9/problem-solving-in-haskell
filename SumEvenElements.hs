--sumEven :: [Integer] -> Integer
--sumEven xs =
--  sumE 0 xs
--      where 
--          sumE acc [] = acc
--          sumE acc (z:zs) = 
--                if even z 
--                then sumE (acc+z) zs 
--                else sumE acc zs

sumEven :: [Integer] -> Integer
sumEven list = sumEven' 0 list

sumEven' :: Integer -> [Integer] -> Integer
sumEven' acc [] = acc
sumEven' acc (h:t) =
    if even h 
    then sumEven' (acc + h) t 
    else sumEven' acc t