{-
TODO: Smart string constructor, numeric only & filtered respectively
-}

prodChar :: Int -> Int
prodChar =
    let digitizer = map $ read . (:[])
    in product . digitizer . show

mulper :: Int -> [Int]
mulper = iterate $ prodChar

mulperCount :: Int -> Int
mulperCount x
    | digits x == 1 = 0
    | otherwise = predIndex mulpers $ (==1) . digits
    where
        digits = length . show
        mulpers = mulper x

predIndex :: [a] -> (a -> Bool) -> Int
predIndex [] _ = 0
predIndex (x:xs) p
    | not $ p x = 1 + predIndex xs p
    | otherwise  = 0