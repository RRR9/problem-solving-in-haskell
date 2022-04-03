polyDerivative :: (Integral a) => [a] -> [a]
polyDerivative list = polyDerivative' list []

polyDerivative' :: (Integral a) => [a] -> [a] -> [a]
polyDerivative' [a] l = l
polyDerivative' list l = polyDerivative' (tail list) (((head list) * (fromIntegral(length list) - 1)) : l)