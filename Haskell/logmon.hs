newtype Logged w a = Logged { runLogged :: (a, w)} deriving Show

instance Functor (Logged w) where
    fmap f (Logged (x, log)) = Logged (f x, log)

instance Monoid w => Applicative (Logged w) where
    pure x = Logged (x, mempty) -- provide new log
    -- breaking rules?
    mf <*> ma =
       Logged ((f a), log1 `mappend` log2) -- maintain both logs?
           where (f, log1) = runLogged mf
                 (a, log2) = runLogged ma

instance Monoid w => Monad (Logged w) where
    ma >>= fmb =
        Logged (b, log1 `mappend` log2) -- also maintain both logs?
            where (x, log1) = runLogged ma
                  (b, log2) = runLogged (fmb x)

tell :: Monoid w => w -> Logged w ()
tell str = Logged ((), str)

reversed' :: Show a => [a] -> Logged [[a]] [a]
reversed' [] = return []
reversed' (x:xs) =
    do
        inner <- reversed' xs
        -- >>= passes log along
        let rslt = inner ++ [x]
         in do
             tell [rslt]
             return rslt

main = do
    putStrLn ("in: " ++ show xs)
    print log
        where
            xs = [1, 2, 3, 4]
            log = reversed' xs
