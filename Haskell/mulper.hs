{-
TODO: Smart string constructor, numeric only & filtered respectively
-}

prodChar :: String -> Int
prodChar xs =  product [read [x] :: Int| x <- xs]

mulper :: Int -> [Int]
mulper = iterate $ prodChar . show

mulperCount :: Int -> Int
mulperCount x
    | digits x == 1 = 0
    | otherwise = predIndex mulpers (\d -> digits d == 1)
    where
        digits = length . show
        mulpers = mulper x

predIndex :: [a] -> (a -> Bool) -> Int
predIndex [] _ = 0
predIndex (x:xs) p
    | not $ p x = 1 + predIndex xs p
    | otherwise  = 0