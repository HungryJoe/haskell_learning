import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Char (toUpper)
import System.Environment (getArgs)


main = do
    args <- getArgs
    file <- readFile (head args)
    let parsedFile = parseFile file
    let solution = sum $ determineDisplayedValues parsedFile
    print $ head parsedFile
    print solution


data Segment = A | B | C | D | E | F | G deriving (Eq, Show, Ord, Read)
type Digit = S.Set Segment

parseFile :: String -> [([Digit], [Digit])]
parseFile file = map parseLine $ lines file
    where parseLine line = (map fromJust $ takeWhile (/= Nothing) segmentedLine, map fromJust $ tail $ dropWhile (/= Nothing) segmentedLine)
              where segmentedLine = segmentLine line
segmentLine line = foldr addDigit [] $ words line
    where addDigit "|" digits = Nothing : digits
          addDigit word digits = Just (S.fromList (map (read . (:[]) . toUpper) word)) : digits

determineDisplayedValues :: [([Digit], [Digit])] -> [Int]
determineDisplayedValues = map determineDisplayedValue
    where determineDisplayedValue (digits, display) = computeValue display $ solveDisplay digits

solveDisplay :: [Digit] -> M.Map Digit Int
solveDisplay digits = invertMap $ passThree $ passTwo $ passOne digits
    where invertMap curMap = M.foldlWithKey (\newMap key val -> M.insert val key newMap) M.empty curMap

passOne :: [Digit] -> (M.Map Int Digit, [Digit])
passOne [] = (M.empty, [])
passOne (digit:digits)
    | S.size digit == 2 = insert 1
    | S.size digit == 3 = insert 7
    | S.size digit == 4 = insert 4
    | S.size digit == 7 = insert 8
    | otherwise = (recurMap, digit : recurDigits)
    where (recurMap, recurDigits) = passOne digits
          insert number = (M.insert number digit recurMap, recurDigits)

passTwo :: (M.Map Int Digit, [Digit]) -> (M.Map Int Digit, [Digit])
passTwo (digMap, []) = (digMap, [])
passTwo (digMap, digit:digits)
    | S.size digit == 5 && M.member 1 recurMap && M.member 4 recurMap && passesTestFor 5 = insert 5
    | S.size digit == 5 && M.member 1 recurMap && M.member 4 recurMap && M.member 7 recurMap && passesTestFor 2 = insert 2
    | S.size digit == 5 && M.member 7 recurMap && passesTestFor 3 = insert 3
    | otherwise = (recurMap, digit:recurDigits)
    where (recurMap, recurDigits) = passTwo (digMap, digits)
          insert number = (M.insert number digit recurMap, recurDigits)
          passesTestFor 5 = (recurMap M.! 4) S.\\ (recurMap M.! 1) `S.isProperSubsetOf` digit
          passesTestFor 2 = not ((recurMap M.! 4) S.\\ (recurMap M.! 1) `S.isSubsetOf` digit) && not ((recurMap M.! 7) `S.isSubsetOf` digit)
          passesTestFor 3 = (recurMap M.! 7) `S.isSubsetOf` digit
          passesTestFor n = error $ "Tried to test " ++ show n ++ " on pass 2"

passThree :: (M.Map Int Digit, [Digit]) -> M.Map Int Digit
passThree (digMap, []) = digMap
passThree (digMap, digit:digits)
    | S.size digit == 6 && M.member 4 recursion && passesTestFor 9 = insert 9
    | S.size digit == 6 && M.member 5 recursion && passesTestFor 0 = insert 0
    | S.size digit == 6 && M.member 5 recursion && M.member 1 recursion && passesTestFor 6 = insert 6
    | otherwise = error $ "Pass three encountered unfamiliar digit: " ++ show digit ++ "\nRemaining digits are: " ++ show digits ++ "\nMap looks like: " ++ show digMap
    where recursion = passThree (digMap, digits)
          insert number = M.insert number digit recursion
          passesTestFor 9 = (recursion M.! 4) `S.isProperSubsetOf` digit
          passesTestFor 0 = not $ (recursion M.! 5) `S.isSubsetOf` digit
          passesTestFor 6 = (recursion M.! 5) `S.isSubsetOf` digit && not ((recursion M.! 1) `S.isSubsetOf` digit)
          passesTestFor n = error $ "Tried to test " ++ show n ++ " on pass 3"

computeValue :: [Digit] -> M.Map Digit Int -> Int
computeValue display digitIntMap = sum [parseDigit digit * pow | (digit, pow) <- zippedDisplay]
    where zippedDisplay = zip display [1000, 100, 10, 1]
          parseDigit digit = fromJust (M.lookup digit digitIntMap)
