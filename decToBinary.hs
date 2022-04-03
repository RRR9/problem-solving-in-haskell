import Data.Char

intToBin :: Int -> [Char]
intToBin n
    | n > 0 = intToBin (n `div` 2) ++ chr (n `mod` 2 + 48) : []
    | n == 0 = ['0']