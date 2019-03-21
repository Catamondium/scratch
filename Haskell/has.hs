-- Comment
{-
    multiline/inline comment
-}
                    -- Str -> Str
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- Integer: Arbitrarily large integer, inefficient
factorial :: Integer -> Integer
factorial n = product [1..n]

head' :: [a] -> a
head' []    = error "Empty list" -- throws
-- automatically bind 'x' head, ignore rest '_'
head' (x:_) = x

-- Other pattern matching
-- Integral typeclass(interface), Integers incl 0
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky SEVEN"
lucky x = "Unlucky"

-- Numeric typeclass, processing pairs(2-tuples)
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- '_' ignore param examples, also works in li comp
first :: (a, b, c) -> a
-- Don't even bind other 2
first (x, _, _)     = x

second :: (a, b, c) -> b
second (_, y, _)    = y

third :: (a, b, c) -> c
third (_, _, z)     = z

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- all@ overall reference to match
capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "First is " ++ [x] ++ " for " ++ all

-- Guards, a case equiv
max' :: Ord a => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

-- Where usage, substitutive binding
bmiTell :: RealFloat a => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "Underweight"
    | bmi <= normal = "Normie"
    | bmi <= fat    = "Overwheight"
    | otherwise     = "Whale scale"
    where
        bmi                     = weight / height ^ 2
        (skinny, normal, fat)   = (18.5, 25.0, 30.0)

-- let [bindings] in [block] has block scoped bindings
-- let is also expression

describeList :: [a] -> String
describeList xs = "List is " ++ case xs of
    []  -> "empty"
    [x] -> "singleton"
    xs  -> "long"

-- Recursion! better git gud
maximum' :: Ord a => [a] -> a
maximum' [] = error "Maxing empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail   = x
    | otherwise     = maxTail
    where maxTail   = maximum' xs

-- also obviously definable with max

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ []           = []
zip' [] _           = []
zip' (x:xs) (y:ys)  = (x,y):zip' xs ys
