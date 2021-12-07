import Data.Maybe (isJust, fromJust)
import Data.List (transpose, delete)
import System.Environment (getArgs)
import qualified Data.Set as Set


main = do
    args <- getArgs
    file <- readFile (head args)
    let (draws, boards) = parseFile file
    let winner@(winningDraw, winningBoard) = playToWin boards draws
    let solution1 = calculateScore winningDraw winningBoard
    let (losingDraw, losingBoards) = playToLose boards draws
    let solution2 = calculateScore losingDraw (head losingBoards)
    print winner
    print solution1
    print (losingDraw, losingBoards)
    print solution2


newtype BingoBoard = BingoBoard [BingoLine] deriving Eq
instance Show BingoBoard where
    show (BingoBoard []) = "[]"
    show (BingoBoard lines) = '[' : foldl showLine "" lines ++ "]"
        where showLine acc line = acc ++ show line ++ ",\n"
instance Ord BingoBoard where
    compare (BingoBoard a) (BingoBoard b)
        | a == b = EQ
        | otherwise = head compareBBs 
        where compareBBs = dropWhile (==EQ) $ zipWith compare a b
deconstructBB :: BingoBoard -> [[BingoSquare]]
deconstructBB (BingoBoard b) = b
type BingoLine = [BingoSquare]
data BingoSquare = BingoSquare{value :: Int, marked :: Bool} deriving (Eq, Ord)
instance Show BingoSquare where
    show BingoSquare{marked=m, value=v}= '(' : show m ++ ", " ++ show v ++ ")"

parseFile :: String -> ([Int], [BingoBoard])
parseFile file = (parseDraws (head lines'), parseBoards (tail lines') [])
    where parseDraws draws = read (('[': draws) ++ "]") :: [Int]
          lines' = lines file

parseBoards :: [String] -> [BingoBoard] -> [BingoBoard]
parseBoards [] boards = boards
parseBoards ("":one:two:three:four:five:otherBoards) prevBoards = parseBoards otherBoards (parsedBoard : prevBoards)
    where parsedBoard = BingoBoard [parseLine one, parseLine two, parseLine three, parseLine four, parseLine five]
          parseLine line = map (bsFromInt . read) (words line)
          bsFromInt x = BingoSquare{marked=False, value=x}
parseBoards rest boards = error $ "Failed parsing when boards was " ++ show boards ++ " and rest was " ++ show rest

findNextWinner :: [BingoBoard] -> [Int] -> Maybe (Int, [BingoBoard], [BingoBoard], [Int])
findNextWinner _ [] = Nothing
findNextWinner boards (x:xs)
    | not $ null winners = Just (x, winners, markedBoards, xs)
    | otherwise = findNextWinner markedBoards xs
    where winners = map BingoBoard $ checkBoards (map deconstructBB markedBoards) []
          checkBoards [] winners = winners
          checkBoards (board:boards') winners
            | checkBoard board = checkBoards boards' $ board : winners
            | otherwise = checkBoards boards' winners
          checkBoard board = checkRows board || checkCols board
          checkRows = any (all marked)
          checkCols = any (all marked) . transpose
          markedBoards = map (BingoBoard . markBoard x . deconstructBB) boards
          markBoard y = map (map $ updateSquare y)
          updateSquare y square@BingoSquare{marked=marked, value=value}
            | not marked && y == value = square{marked=True}
            | otherwise = square

playToWin :: [BingoBoard] -> [Int] -> (Int, BingoBoard)
playToWin boards draws = (winningDraw, head winningBoard)
    where (winningDraw, winningBoard, _, _) = fromJust $ findNextWinner boards draws

playToLose :: [BingoBoard] -> [Int] -> (Int, [BingoBoard])
playToLose boards draws = go boards draws []
    where go :: [BingoBoard] -> [Int] -> [(Int, [BingoBoard])] -> (Int, [BingoBoard])
          go _ [] winners = error "Ran out of draws before finding the last winner"
          go [] _ winners = head winners
          go boards' draws' winners
            | isJust maybeWinner = go diffedBoards remainingDraws ((winningDraw, winningBoards) : winners)
            | otherwise = go boards' [] winners
            where maybeWinner = findNextWinner boards' draws'
                  (winningDraw, winningBoards, markedBoards, remainingDraws) = fromJust maybeWinner
                  diffedBoards = Set.toList $ Set.difference (Set.fromList markedBoards) (Set.fromList winningBoards)

calculateScore :: Int -> BingoBoard -> Int
calculateScore draw board = draw * sumUnmarked
          where sumUnmarked = sum [sum [value square | square <- line, not (marked square)] | line <- deconstructBB board]
