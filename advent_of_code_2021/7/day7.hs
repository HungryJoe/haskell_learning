import Data.List (sort)
import System.Environment (getArgs)


main = do
    args <- getArgs
    file <- readFile (head args)
    let parsedFile = parseFile file
    let solution1 = sumCosts parsedFile $ median parsedFile
    let solution2 = sumCostsExp parsedFile (average parsedFile)
    print solution1
    print solution2


parseFile :: String -> [Int]
parseFile file = read ('[' : file ++ "]") :: [Int]

median :: [Int] -> Int
median xs
    | len `mod` 2 == 1 = sorted !! halfLen
    | otherwise = ((sorted !! halfLen - 1) + (sorted !! halfLen)) `div` 2 + 1
    where len = length xs
          halfLen = len `div` 2
          sorted = sort xs

average :: [Int] -> Int
average xs = sum xs `div` length xs

sumCosts :: [Int] -> Int -> Int
sumCosts xs med = sum [abs (x - med) | x <- xs]

sumCostsExp :: [Int] -> Int -> Int
sumCostsExp xs avg = sum $ [sumToI $ abs (x - avg) | x <- xs]
    where sumToI i = sum [1..i]
