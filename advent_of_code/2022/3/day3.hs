import Data.Char (isLower, isUpper)
import System.Environment (getArgs)
type Rucksack = ([Int], [Int])

main = do
    args <- getArgs
    let fileName = head args
    fileContents <- readFile fileName
    let rucksacks = map parseLine $ lines fileContents
    let commonPriorities = findCommonPriorities rucksacks
    print $ sum commonPriorities

charToPriority :: Char -> Int
charToPriority a
    | isLower a = fromEnum a - fromEnum 'a' + 1
    | isUpper a = fromEnum a - fromEnum 'A' + 27

parseLine :: String -> Rucksack
parseLine line = splitAt (div (length line) 2) $ map charToPriority line

findCommonPriority :: Rucksack -> Int
findCommonPriority (a:as, bs)
    | a `elem` bs = a
    | otherwise = findCommonPriority (as, bs)

findCommonPriorities :: [Rucksack] -> [Int]
findCommonPriorities = map findCommonPriority
