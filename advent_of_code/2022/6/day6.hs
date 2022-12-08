import Data.List (nub)
import System.Environment (getArgs)

main = do
    args <- getArgs
    let fileName = head args
    fileContents <- readFile fileName
    print $ indexOfEndOfFirstNsomeOfUniqueCharacters fileContents 4
    print $ indexOfEndOfFirstNsomeOfUniqueCharacters fileContents 14

indexOfEndOfFirstNsomeOfUniqueCharacters :: String -> Int -> Int
indexOfEndOfFirstNsomeOfUniqueCharacters str n = go (drop n str) n $ take n str
    where go (x:xs) i nubCandidate
            | nub nubCandidate == nubCandidate = i
            | otherwise = go xs (i+1) $ tail nubCandidate ++ [x]
