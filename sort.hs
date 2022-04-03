quickSort :: Ord a => [a]->[a]
quickSort [] = []
quickSort (x:[]) = [x]
quickSort (x:y:[]) =
    if x <= y
    then x:y:[]
    else y:x:[]
quickSort (h:t) =
    let 
        pivot = h
        leftPart = filter (<= h) t
        rightPart = filter (> h) t
    in
        quickSort leftPart ++ [h] ++ quickSort rightPart

directSort :: Ord a => [a]->[a]
directSort [] = []
directSort (x:[]) = [x]
directSort (x:y:[]) =
    if x <= y
    then x:y:[]
    else y:x:[]
directSort l =
    let 
        m = minimum l
        (part0, part1) = span (/=m) l
    in
        m : directSort (part0 ++ tail part1)

