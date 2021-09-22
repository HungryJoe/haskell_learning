import Data.List
import Data.Maybe

-- 22
testRange =
    range 4 9 == [4,5,6,7,8,9]
    && range 1 10 == [1..10]
    && range 30 40 == [30..40]
range :: Integral a => a -> a -> [a]
range start end = takeWhile (<= end) $ iterate (+ 1) start

-- 26
-- Enumeration algorithm from: https://en.wikipedia.org/wiki/Combination#Enumerating_k-combinations
combinations :: Int -> [a] -> [[a]]
combinations k list = map (map snd) $ go initial
    where initial = take k zipped
          zipped = zip [1..] list
          go [] = []
          go comb = comb : go (next_comb comb)
          next_comb comb
            | last_idx /= k = take (k - 1) comb ++ [zipped !! (last_idx + 1)]
            | isJust next_start = let start = fromJust next_start in take start comb ++ take (k - start) (filter (\(y, _) -> y > start) zipped) 
            | otherwise = []
            where last_idx = fst (last comb)
                  next_start = if null nexts then Nothing else Just (fst $ last nexts)
                    where cmp (i, _) = i < (fst (comb !! (i + 1)) - 1)
                          nexts = filter cmp comb
