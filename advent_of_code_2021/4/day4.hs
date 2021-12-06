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
    let ((losingDraw, losingBoard), lenBoards) = playToLose boards draws
    let solution2 = calculateScore losingDraw losingBoard
    print winner
    print solution1
    print lenBoards
    print solution2


type BingoBoard = [BingoLine]
type BingoLine = [BingoSquare]
data BingoSquare = BingoSquare{marked :: Bool, value :: Int} deriving (Show, Eq)

parseFile :: String -> ([Int], [BingoBoard])
parseFile file = (parseDraws (head lines'), parseBoards (tail lines') [])
    where parseDraws draws = read (('[': draws) ++ "]") :: [Int]
          lines' = lines file

parseBoards :: [String] -> [BingoBoard] -> [BingoBoard]
parseBoards [] boards = boards
parseBoards ("":one:two:three:four:five:otherBoards) prevBoards = parseBoards otherBoards ([parseBoardLine one, parseBoardLine two, parseBoardLine three, parseBoardLine four, parseBoardLine five] : prevBoards)
    where parseBoardLine line = map (bsFromInt . read) (words line)
          bsFromInt x = BingoSquare{marked=False, value=x}
parseBoards rest boards = error $ "Failed parsing when boards was " ++ show boards ++ " and rest was " ++ show rest

findNextWinner :: [BingoBoard] -> [Int] -> Maybe (Int, BingoBoard, [BingoBoard], [Int])
findNextWinner _ [] = Nothing
findNextWinner [] _ = Nothing
findNextWinner boards (x:xs)
    | isJust winner = Just (x, fromJust winner, markedBoards, xs)
    | otherwise = findNextWinner markedBoards xs
    where winner = checkBoards markedBoards
          checkBoards [] = Nothing
          checkBoards (board:boards') 
            | checkBoard board = Just board 
            | otherwise = checkBoards boards'
          checkBoard board = checkRows board || checkCols board
          checkRows = any (all marked)
          checkCols = any (all marked) . transpose
          markedBoards = map (markBoard x) boards
          markBoard y = map (map $ updateSquare y)
          updateSquare y square@BingoSquare{marked=marked, value=value}
            | not marked && y == value = square{marked=True}
            | otherwise = square


playToWin boards draws = (winningDraw, winningBoard)
    where (winningDraw, winningBoard, _, _) = fromJust $ findNextWinner boards draws

playToLose :: [BingoBoard] -> [Int] -> ((Int, BingoBoard), Int)
playToLose boards draws = go boards draws [] (length boards)
    where go :: [BingoBoard] -> [Int] -> [(Int, BingoBoard)] -> Int -> ((Int, BingoBoard), Int)
          go _ [] winners len = (head winners, len)
          go [] _ winners len = (head winners, len)
          go boards' draws' winners len
            | isJust maybeWinner = go (delete winningBoard markedBoards) remainingDraws ((winningDraw, winningBoard) : winners) (length markedBoards)
            | otherwise = go boards' [] winners len
            where maybeWinner = findNextWinner boards' draws'
                  (winningDraw, winningBoard, markedBoards, remainingDraws) = fromJust maybeWinner

calculateScore :: Int -> BingoBoard -> Int
calculateScore draw board = draw * sumUnmarked
          where sumUnmarked = sum [sum [value square | square <- line, not (marked square)] | line <- board]
