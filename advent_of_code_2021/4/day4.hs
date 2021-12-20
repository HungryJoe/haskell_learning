import Data.List (transpose, delete)
import System.Environment (getArgs)
import qualified Data.Set as Set


main = do
    args <- getArgs
    file <- readFile (head args)
    let (draws, bingoLines) = parseFile file
    let (winningDraw, winningBoardsUnmarkedSquares) = playToWin draws bingoLines
    let (losingDraw, losingBoardsUnmarkedSquares) = playToLose draws bingoLines
    let solution1 = calculateScore winningDraw winningBoardsUnmarkedSquares
    let solution2 = calculateScore losingDraw losingBoardsUnmarkedSquares
    print solution1
    print solution2


type BingoBoard = [[Int]]
data BingoLine = BingoLine{board :: Int, line :: [Int]} deriving Show

parseFile :: String -> ([Int], [BingoLine])
parseFile file = (parseDraws (head lines'), parseBoards (tail lines') [])
    where parseDraws draws = read (('[': draws) ++ "]") :: [Int]
          lines' = lines file

parseBoards :: [String] -> [BingoLine] -> [BingoLine]
parseBoards [] bingoLines = bingoLines
parseBoards ("":one:two:three:four:five:otherBoards) prevBingoLines = parseBoards otherBoards (parsedBingoLines ++ prevBingoLines)
    where parsedBoard = [parseLine one, parseLine two, parseLine three, parseLine four, parseLine five]
          parsedBingoLines = map (BingoLine $ length otherBoards) $ parsedBoard ++ transpose parsedBoard
          parseLine line = map read (words line)
parseBoards rest boards = error $ "Failed parsing when boards was " ++ show boards ++ " and rest was " ++ show rest

-- @return the winning draw and the values left unmarked on the winning board(s)
playToWin :: [Int] -> [BingoLine] -> (Int, [Set.Set Int])
playToWin [] _ = error "Ran out of draws"
playToWin (draw:draws) bingoLines
    | not $ Set.null winningBoards = (draw, Set.toList $ Set.map (gatherUnmarkedSquaresForBoard updatedLines) winningBoards)
    | otherwise = playToWin draws updatedLines
    where winningBoards = findWinningBoards updatedLines
          updatedLines = updateLines bingoLines draw

-- @return the winning draw and the values left unmarked on the winning board(s)
playToLose :: [Int] -> [BingoLine] -> (Int, [Set.Set Int])
playToLose [] bingoLines = error $ "Ran out of draws with " ++ show bingoLines ++ " lines left"
playToLose (draw:draws) bingoLines
    | onlyOneBoard && not (Set.null winningBoards) = (draw, Set.toList $ Set.map (gatherUnmarkedSquaresForBoard updatedLines) winningBoards)
    | otherwise = playToLose draws updatedLines
    where winningBoards = findWinningBoards updatedLines
          updatedLines = updateLines bingoLines draw
          onlyOneBoard = Set.size (Set.fromList $ map board updatedLines) == 1

findWinningBoards :: [BingoLine] -> Set.Set Int
findWinningBoards bingoLines = Set.fromList [board bLine | bLine <- bingoLines, null (line bLine)]

updateLines :: [BingoLine] -> Int -> [BingoLine]
updateLines bingoLines draw = [removeDraw bingoLine | bingoLine <- bingoLines, not $ board bingoLine `Set.member` boardsThatWon]
    where removeDraw bingoLine@BingoLine{board=_, line=l} = bingoLine{line = delete draw l}
          boardsThatWon = Set.fromList $ [board bingoLine | bingoLine <- bingoLines, null $ line bingoLine]

gatherUnmarkedSquaresForBoard :: [BingoLine] -> Int -> Set.Set Int
gatherUnmarkedSquaresForBoard bingoLines boardId = foldl addLineToSet (Set.empty :: Set.Set Int) $ filter (hasBoard boardId) bingoLines
    where hasBoard b bingoLine = b == board bingoLine
          addLineToSet set bingoLine = set `Set.union` Set.fromList (line bingoLine)

calculateScore :: Int -> [Set.Set Int] -> Int
calculateScore draw [winnerUnmarked] = draw * Set.foldl (+) 0 winnerUnmarked
calculateScore _ winnersUnmarked = error $ "Should be exactly one first/last winner, found " ++ show (length winnersUnmarked)
