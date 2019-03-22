trans :: Eq a => [a] -> [a] -> [a] -> [a]
trans [] _ str = str
trans _ [] str = str
trans _ _ [] = []
trans f t (c:str)
    | c `elem` cropf    = sub   : trans cropf t str
    | otherwise         = c     : trans cropf t str
    where
        ft = zip f t
        cropf = take (length t) f -- insure corresponding lengths
        Just sub = lookup' c ft

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ []    = Nothing
lookup' x ((a, b):lst)
    | x == a    = Just b
    | otherwise = lookup x lst