import Data.List (transpose)
import Data.Bifunctor (Bifunctor(first, second, bimap))
import Data.Tuple (Solo(Solo), swap)
import Data.Set (cartesianProduct, fromList, toList)
import System.Environment (getArgs)
type TreeMap = [[Int]]  -- Assume that all inner lists have the same length

main = do
    args <- getArgs
    let fileName = head args
    fileContents <- readFile fileName
    let treeMap = parseFile $ lines fileContents
    let score1 = calculateScore1 treeMap
    let score2 = calculateScore2 treeMap
    print score1
    print score2

parseFile :: [String] -> TreeMap
parseFile = map (map $ read . (:[]))

calculateScore1 :: TreeMap -> Int
calculateScore1 treeMap = length $ filter snd $ getVisibilities treeMap

calculateScore2 :: TreeMap -> Int
calculateScore2 treeMap = maximum $ map snd $ getScenicScores treeMap

getVisibilities :: TreeMap -> [((Int, Int), Bool)]
getVisibilities = mapCombinedOp (||) listIsVisible

getScenicScores :: TreeMap -> [((Int, Int), Int)]
getScenicScores = mapCombinedOp (*) listScenicScore

mapCombinedOp :: (a -> a -> a) -> ([Int] -> Int -> a) -> TreeMap -> [((Int, Int), a)]
mapCombinedOp combiner op treeMap = zip indices $ map (combineOpAt combiner op treeMap) indices
    where indices = toList $ cartesianProduct (fromList [0..length treeMap - 1]) (fromList [0..length (head treeMap) - 1])

combineOpAt :: (a -> a -> a) -> ([Int] -> Int -> a) -> TreeMap -> (Int, Int) -> a
combineOpAt combiner op treeMap index = combiner (oneWayOp op treeMap index 0) (oneWayOp op (transpose treeMap) (swap index) 0)

oneWayOp :: ([Int] -> Int -> a) -> TreeMap -> (Int, Int) -> Int -> a
oneWayOp op (row:rest) (x, y) n
    | x == n = op row y
    | otherwise = oneWayOp op rest (x, y) (n + 1)

listIsVisible :: [Int] -> Int -> Bool
listIsVisible row x = uncurry eithermaxEqHead $ first (row!!x:) $ splitAt x row
    where maxEqHead l = isUnique l (head l) && maximum l == head l
          eithermaxEqHead l m = maxEqHead l || maxEqHead m

isUnique :: Eq a => [a] -> a -> Bool
isUnique xs y = length (filter (==y) xs) == 1

listScenicScore :: [Int] -> Int -> Int
listScenicScore row x = productViewingDistances (source, firstRay) (source, secondRay)
    where (firstRay, secondRay) = bimap reverse (drop 1) (splitAt x row)
          source = row !! x
          viewingDistance (y, l) = shorterTreesDist + (if shorterTreesDist < length l then 1 else 0)
            where shorterTreesDist = length $ takeWhile (< y) l
          productViewingDistances l m = viewingDistance l * viewingDistance m
