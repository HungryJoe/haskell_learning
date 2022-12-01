

parseFile :: String -> [Int]
parseFile = concatMap (map length . words . dropWhile (/= '|')) . lines

count1478 :: [Int] -> Int
count1478 digitLengths = length [len | len <- digitLengths, len `elem` [2, 3, 4, 7]]
