import Data.List (find, elemIndex)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

-- For a Range (a,b), we assume that a <= b
type Range = (Int, Int) 

main = do
    args <- getArgs
    let fileName = head args
    fileContents <- readFile fileName
    let lines' = lines fileContents
    let pairs = map parseLine lines'
    let overlaps1 = map doCompletelyOverlap pairs
    let overlaps2 = map doOverlap pairs
    print $ calculateScore overlaps1
    print $ calculateScore overlaps2

parseLine :: String -> (Range, Range)
parseLine line = (parseRange firstHalf, parseRange secondHalf)
    where (firstHalf, ',':secondHalf) = splitAt (fromJust $ elemIndex ',' line) line

parseRange :: String -> Range
parseRange rng = (read low, read high)
    where (low, '-':high) = splitAt (fromJust $ elemIndex '-' rng) rng

doCompletelyOverlap :: (Range, Range) -> Bool
doCompletelyOverlap (one, two) = doCompletelyOverlapOneDir one two || doCompletelyOverlapOneDir two one
    where doCompletelyOverlapOneDir (e,f) (g,h) = e <= g && f >= h

doOverlap :: (Range, Range) -> Bool
doOverlap ((a,b), (c,d)) = not $ b < c || d < a

calculateScore :: [Bool] -> Int
calculateScore = length . filter id
