import Data.List (sortOn)
import System.Environment (getArgs)
import Data.Ord (Down(Down))

main = do
    args <- getArgs
    let fileName = head args
    fileContents <- readFile fileName
    let elves = readElves $ lines fileContents
    let maxElves = findMaxElves elves 3
    -- putStrLn $ foldl1 ((++) . (++"\n")) $ map show elves
    let maxSums = map sum maxElves
    print maxSums
    print $ sum maxSums

findMaxElves :: [[Float]] -> Int -> [[Float]]
findMaxElves elves cnt = take cnt $ sortOn (Down . sum) elves

readElves :: [String] -> [[Float]]
readElves (line:rest)
    | line == "" = [] : recur
    | otherwise = ((read line :: Float) : head recur) : tail recur
    where recur = readElves rest
readElves [] = []
