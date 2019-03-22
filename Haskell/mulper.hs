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
    | otherwise = counter $ mulpers
    where
        digits = length . show
        counter = (+1) . length . takeWhile ((/=1) . digits)
        mulpers = mulper x

-- Fundamentally takeWhile?
predIndex :: (a -> Bool) -> [a] -> Int
predIndex _ [] = 0
predIndex p (x:xs)
    | not $ p x = 1 + predIndex p xs
    | otherwise  = 0