import System.Environment (getArgs)
import Data.List (transpose)
import Data.Char (digitToInt)
import Data.Bits


main = do
    args <- getArgs
    file <- readFile $ head args
    let solution = calculateGammaEpsilon $ findModes $ parseFile file
    let o2Rating = bitStringToInt $ findRating True $ parseFile file
    let co2Rating = bitStringToInt $ findRating False $ parseFile file
    print solution
    print (o2Rating, co2Rating)

-- True == 0, False == 1
type BitString = [Bool]

parseFile :: String -> [BitString]
parseFile file = map (map charToBool) $ lines file
    where charToBool '0' = True
          charToBool '1' = False
          charToBool ch = error $ "Expected '0' or '1', got " ++ [ch]

findModes :: [BitString] -> BitString
findModes bitstrings = [findMode col | col <- transpose bitstrings]

findMode :: BitString -> Bool
findMode col = num0s > num1s
    where num0s = length $ [ch | ch <- col, ch]
          num1s = length $ [ch | ch <- col, not ch]

flipBits :: BitString -> BitString
flipBits str = [not ch | ch <- str]

calculateGammaEpsilon :: BitString -> (Int, Int)
calculateGammaEpsilon modes = (bitStringToInt modes, bitStringToInt $ flipBits modes)

-- Source: https://stackoverflow.com/a/22629708
bitStringToInt :: BitString -> Int
bitStringToInt bits = sum $ zipWith toDec (reverse bits) [0 .. length bits]
    where toDec base exp 
            | base = 0
            | otherwise = 2 ^ exp

findRating :: Bool -> [BitString] -> BitString
findRating isO2 bitstrings = filterOnModeAt bitstrings 0
    where filterOnModeAt :: [BitString] -> Int -> BitString
          filterOnModeAt [] _ = error "Oops"
          filterOnModeAt [x] _ = x
          filterOnModeAt xs idx 
            | idx >= length (head xs) = error $ "Index " ++ show idx ++ " exceeds number of columns in matrix: " ++ show xs
            | otherwise = filterOnModeAt [x | x <- xs, x !! idx == findMode' (transpose xs !! idx)] (idx + 1)
          findMode'
            | isO2 = findMode
            | otherwise = not . findMode
