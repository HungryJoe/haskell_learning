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
combinations k list = map (map snd) $ go (take k zipped)
    where zipped = zip [0..] list
          go [] = []
          go comb = comb : go (nextComb comb zipped)

nextComb :: [(Int, a)] -> [(Int, a)] -> [(Int, a)]
nextComb comb zipped
    | last_idx < (length zipped - 1) = take (k - 1) comb ++ [zipped !! (last_idx + 1)]
    | isJust next_start =
        let start = fromJust next_start in
        take start comb ++ take (k - start) (filter ((> start) . fst) zipped) 
    | otherwise = []
    where last_idx = fst (last comb)
          k = length comb
          next_start = if null nexts then Nothing else Just (fst $ last nexts)
              where cmp ((x, _), i) = x < (fst (comb !! (i + 1)) - 1)
                    nexts = map fst $ filter cmp $ zip (init comb) [0..]

-- <comb zipped nexts next_start>
-- [0, 2, 4] [0, 1, 2, 3, 4] -> [0, 2, 4] [0..4] [(0, 0), (1, 2)]
--                           -> [0, 2, 4] [0..4] [(0, 0), (2, 1)]
--                           -> ---------------- [(0, 0), (2, 1)]
--                           -> ---------------- [] 2
--                           -> [0,2] ++ [3] [0..4] 2
--                           -> [0,2,3] [0..4]
--
-- [0,2,3] [0..4] -> 
