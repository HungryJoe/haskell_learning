import Data.List (transpose, delete)
import System.Environment (getArgs)
import qualified Data.Set as Set


main = do
    args <- getArgs
    file <- readFile (head args)
    let (draws, bingoLines) = parseFile file
    let (winningDraw, winningBoardsUnmarkedSquares) = playBingo True draws bingoLines
    let (losingDraw, losingBoardsUnmarkedSquares) = playBingo False draws bingoLines
    let solution1 = calculateScore winningDraw winningBoardsUnmarkedSquares
    let solution2 = calculateScore losingDraw losingBoardsUnmarkedSquares
    print solution1
    print solution2


type BingoBoard = [[Int]]
data BingoLine = BingoLine{board :: Int, line :: [Int]} deriving Show
type IntSet = Set.Set Int

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
playBingo :: Bool -> [Int] -> [BingoLine] -> (Int, [IntSet])
playBingo _ [] _ = error "Ran out of draws"
playBingo isForFirst (draw:draws) bingoLines
    | (isForFirst || onlyOneBoard) && not (Set.null winningBoards) = (draw, unmarkedSquaresOfWinners)
    | otherwise = playBingo isForFirst draws updatedLines
    where winningBoards = findWinningBoards updatedLines
          updatedLines = updateLines bingoLines draw
          onlyOneBoard = Set.size (Set.fromList $ map board updatedLines) == 1
          unmarkedSquaresOfWinners = Set.toList $ Set.map (gatherUnmarkedSquaresForBoard updatedLines) winningBoards

findWinningBoards :: [BingoLine] -> IntSet
findWinningBoards bingoLines = Set.fromList [board bLine | bLine <- bingoLines, null (line bLine)]

updateLines :: [BingoLine] -> Int -> [BingoLine]
updateLines bingoLines draw = [removeDraw bingoLine | bingoLine <- bingoLines, not $ board bingoLine `Set.member` boardsThatWon]
    where removeDraw bingoLine@BingoLine{board=_, line=l} = bingoLine{line = delete draw l}
          boardsThatWon = Set.fromList $ [board bingoLine | bingoLine <- bingoLines, null $ line bingoLine]

gatherUnmarkedSquaresForBoard :: [BingoLine] -> Int -> IntSet
gatherUnmarkedSquaresForBoard bingoLines boardId = foldl addLineToSet (Set.empty :: IntSet) $ filter (hasBoard boardId) bingoLines
    where hasBoard b bingoLine = b == board bingoLine
          addLineToSet set bingoLine = set `Set.union` Set.fromList (line bingoLine)

calculateScore :: Int -> [IntSet] -> Int
calculateScore draw [winnerUnmarked] = draw * Set.foldl (+) 0 winnerUnmarked
calculateScore _ winnersUnmarked = error $ "Should be exactly one first/last winner, found " ++ show (length winnersUnmarked)
