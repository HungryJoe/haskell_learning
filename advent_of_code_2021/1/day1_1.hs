import System.Environment


main = do
    args <- getArgs
    file <- readFile (head args)
    let intLines = map read $ lines file :: [Int]
    let solution = countIncreasing intLines
    print solution

countIncreasing :: [Int] -> Int
countIncreasing list = go list 0
    -- where go :: [Int] -> Int -> Int
    --       go [] n = n
    --       go [x] n = n
    --       go (x:y:xs) n
    --           | y > x = go (y:xs) (n + 1)
    --           | otherwise = go (y:xs) n
    where go :: [Int] -> Int -> Int
          go (x:y:xs) n
              | y > x = go (y:xs) (n + 1)
              | otherwise = go (y:xs) n
          go _ n = n
