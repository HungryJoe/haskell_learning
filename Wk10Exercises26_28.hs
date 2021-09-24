import Data.List
import Data.Maybe

-- 26, take 2
combinations :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations 1 list = map (:[]) list
combinations k list = nubBy listEqual $ genPerms 0
    where addNext i comb = comb ++ [(list \\ comb) !! i]
          listEqual xs ys = and $ zipWith (==) (sort xs) (sort ys)
          recur = combinations (k - 1) list
          n = length list
          genPerms i
            | i > (n - k) = []
            | otherwise = map (addNext i) recur ++ genPerms (i + 1)


numCombinations :: Int -> Int -> Int
numCombinations n k = product [(n - k + 1)..n] `div` fac k

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)
-- [0,1,2] -?> [01, 02, 12]
-- [01, 02, 12] -?> [012]
-- [0,1,2,3] -?> [01,02,03,12,13,23]
--           ->  [01,03,12,23]
