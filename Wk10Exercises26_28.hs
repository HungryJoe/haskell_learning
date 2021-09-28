import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

-- 26, take 2
combinations :: (Eq a, Ord a) => Int -> [a] -> [[a]]
combinations 1 list = map (:[]) list
combinations k list
    | k == n = [list]
    | otherwise = nubBy combEqual $ genPerms 0
    where combEqual xs ys = and $ zipWith (==) (sort xs) (sort ys)
          n = length list
          genPerms i
              | i > (n - k) = []
              | otherwise = map (addNext i) (combinations (k - 1) list) ++ genPerms (i + 1)
              where addNext i comb = (list \\ comb) !! i : comb

-- Favorite solution from the Solutions page
combinations' 1 list = map (:[]) list
combinations' _ [] = [[]]
combinations' k (x:xs) = map (x:) (combinations' (k - 1) xs) ++ combinations' k xs

numCombinations :: Int -> Int -> Int
numCombinations n k = product [(n - k + 1)..n] `div` fac k

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)


-- 27
-- a
group234 :: (Eq a, Ord a) => [a] -> [[[a]]]
group234 list = foldr folder [] $ combinations 2 list
    where folder comb acc = genGroups comb ++ acc
          genGroups comb = map addEndLists $ combinations 3 $ list \\ comb
              where addEndLists comb3 = [comb, comb3, (list \\ comb) \\ comb3]
-- b
groupXYZ :: (Eq a, Ord a) => (Int, Int, Int) -> [a] -> [[[a]]]
groupXYZ (x, y, z) list = foldr folder [] $ combinations x list
    where folder comb acc = concatMap (addNextGroups z) (addNextGroups y [comb]) ++ acc
          addNextGroups k combs = map (: combs) $ combinations k $ foldr (flip (\\)) list combs


-- 28
-- a
sortLen :: (Ord a) => [[a]] -> [[a]]
sortLen = sortOn length
-- b
sortLenFreq :: (Ord a) => [[a]] -> [[a]]
sortLenFreq nestedList = sortOn (flip Map.lookup freqMap . length) nestedList
    where freqMap = Map.fromSet groupLen $ Set.fromList (map length nestedList)
          groupLen len = length $ filter ((len ==) . length) nestedList
