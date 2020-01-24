newtype Logger w a = Logger { runLogger :: (a, w)} deriving Show

instance Functor (Logger w) where
    fmap f (Logger (x, log)) = Logger (f x, log)

instance Monoid w => Applicative (Logger w) where
    pure x = Logger (x, mempty) -- provide new log
    -- breaking rules?
    mf <*> ma =
       Logger ((f a), log1 `mappend` log2) -- maintain both logs?
           where (f, log1) = runLogger mf
                 (a, log2) = runLogger ma

instance Monoid w => Monad (Logger w) where
    ma >>= fmb =
        Logger (b, log1 `mappend` log2) -- also maintain both logs?
            where (x, log1) = runLogger ma
                  (b, log2) = runLogger (fmb x)

tell :: Monoid w => w -> Logger w ()
tell str = Logger ((), str)

reverse' :: Show a => [a] -> Logger [[a]] [a]
reverse' [] = return []
reverse' (x:xs) =
    do
        inner <- reverse' xs
        -- (>>=) passes log
        let rslt = inner ++ [x]
        tell [rslt]
        -- (>>) passes log?
        return rslt

main = do
    putStrLn ("in: " ++ show xs)
    print log
        where
            xs = [1, 2, 3, 4]
            log = reverse' xs
