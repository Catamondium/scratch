trans :: Eq a => [a] -> [a] -> [a] -> [a]
trans _ _ [] = []
trans f t (c:str)
    | c `elem` f    = sub   : trans f t str
    | otherwise     = c     : trans f t str
    where
        ft = zip f t
        Just sub = lookup c ft

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup x ((a, b):lst)
    | x == a    = Just b
    | otherwise = lookup x lst