shift :: [Integer] -> Int -> [Integer]
shift list 0 = list
shift list n = shift (last list : init list) (n - 1)