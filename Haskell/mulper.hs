prodChar :: Int -> Int
prodChar =
    let digitizer = map $ read . (:[])
    in product . digitizer . show

mulper :: Int -> [Int]
mulper = iterate $ prodChar

mulperCount :: Int -> Int
mulperCount x
    | digits x == 1 = 0
    | otherwise     = (+1) . counter . mulper $ x
    where
        digits  = length . show
        counter = length . takeWhile ((/=1) . digits)