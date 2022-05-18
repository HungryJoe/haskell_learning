import Control.Monad.Trans.Writer

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

multWithLog' :: Writer [String] Int
multWithLog' = logNumber 3 >>= (
    \a -> logNumber 5 >>= (
    \b -> tell ["Gonna multiply these two"] >> return (a * b)))

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
instance Monoid (DiffList a) where
    mempty = DiffList id
instance Semigroup (DiffList a) where
    (DiffList xs) <> (DiffList ys) = DiffList (xs . ys)
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 =
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])

finalCountDown' :: Int -> Writer (DiffList String) ()
finalCountDown' 0 = tell (toDiffList ["0"])
finalCountDown' x = finalCountDown' (x-1) >> tell (toDiffList [show x])
