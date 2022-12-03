import System.Environment (getArgs)
main = do
    args <- getArgs
    let fileName = head args
    fileContents <- readFile fileName
    let rounds = parseInput $ lines fileContents
    let score = calculateTotalScore rounds
    print score

data RPSMove = Rock | Paper | Scissors deriving (Show, Eq)
instance Ord RPSMove where
    compare Paper Rock = GT
    compare Rock Paper = LT
    compare Scissors Paper = GT
    compare Scissors Rock = LT
    compare Rock Scissors = GT
    compare Paper Scissors = LT
    compare _ _ = EQ

type RPSRound = (RPSMove, RPSMove)

parseInput :: [String] -> [RPSRound]
parseInput = map parseLine
    where parseLine [theirMove, ' ', myMove] = (parseTheirMove theirMove, parseMyMove myMove)
          parseMyMove 'X' = Rock
          parseMyMove 'Y' = Paper
          parseMyMove 'Z' = Scissors
          parseTheirMove 'A' = Rock
          parseTheirMove 'B' = Paper
          parseTheirMove 'C' = Scissors

calculateRoundScore :: RPSRound -> Int
calculateRoundScore (theirMove, myMove) = scoreMove myMove + scoreRound myMove theirMove
    where scoreMove Rock = 1
          scoreMove Paper = 2
          scoreMove Scissors = 3
          scoreRound a b
            | a > b = 6
            | a < b = 0
            | otherwise = 3

calculateTotalScore :: [(RPSMove, RPSMove)] -> Int
calculateTotalScore = sum . map calculateRoundScore
