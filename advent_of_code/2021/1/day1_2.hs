import System.Environment


main = do
    args <- getArgs
    file <- readFile (head args)
    let intLines = map read $ lines file :: [Int]
    let solution = countIncreasingSums intLines
    print solution

countIncreasingSums :: [Int] -> Int
countIncreasingSums list = go (sumList list []) 0
    where go :: [Int] -> Int -> Int
          go (x:y:xs) n
              | y > x = go (y:xs) (n + 1)
              | otherwise = go (y:xs) n
          go _ n = n


sumList :: [Int] -> [Int] -> [Int]
sumList (x:y:z:xs) sums = sumList (y:z:xs) (sums ++ [x + y + z])
sumList _ sums = sums
