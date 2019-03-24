trans :: Eq a => [a] -> [a] -> [a] -> [a]
trans [] _ seq = seq
trans _ [] seq = seq
trans _ _ [] = []
trans f t (c:seq) =
    let v = lookup' c $ zip f $ cycle t
    in case v of
        Just x  -> x : trans f t seq
        Nothing -> c : trans f t seq

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ []    = Nothing
lookup' x ((a, b):lst)
    | x == a    = Just b
    | otherwise = lookup x lst