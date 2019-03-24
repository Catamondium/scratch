trans :: Eq a => [a] -> [a] -> [a] -> [a]
trans [] _ cs = cs
trans _ [] cs = cs
trans _ _ [] = []
trans from to (c:cs) =
    let v = lookup' c $ zip from $ cycle to
    in case v of
        Just x  -> x : trans from to cs
        Nothing -> c : trans from to cs

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ []    = Nothing
lookup' x ((a, b):lst)
    | x == a    = Just b
    | otherwise = lookup' x lst