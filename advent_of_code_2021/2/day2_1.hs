import System.Environment

main = do
    args <- getArgs
    let fileName = head args
    file <- readFile fileName
    let parsedFile = parseFile file
    let solution = finalPos parsedFile
    print solution

data SubmarineVector = Up Int | Down Int | Forward Int
parseFile :: String -> [SubmarineVector]
parseFile file = map parseLine (lines file)
    where parseLine line = parseVector (words line)
          parseVector [dir, mag]
            | dir == "forward" = Forward  intMag
            | dir == "up" = Up intMag
            | dir == "down" = Down intMag
            | otherwise = error $ "Invalid direction" ++ dir
                where intMag = read mag :: Int
          parseVector list = error $ "Invalid list: " ++ concat list

finalPos :: [SubmarineVector] -> (Int, Int)
finalPos = foldl nextDirection (0, 0)
    where nextDirection (horiz, depth) (Up x) = (horiz, depth - x)
          nextDirection (horiz, depth) (Down x) = (horiz, depth + x)
          nextDirection (horiz, depth) (Forward x) = (horiz + x, depth)
