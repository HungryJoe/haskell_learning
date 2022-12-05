import System.Environment (getArgs)
main = do
    args <- getArgs
    let fileName = head args
    fileContents <- readFile fileName
    let rounds = parseInput $ lines fileContents
    let score = calculateTotalScore rounds
    print score

class (Eq a, Bounded a, Enum a) => Circ a where
    next :: a -> a
    next x
        | x == maxBound = minBound
        | otherwise = succ x
    prev :: a -> a
    prev x
        | x == minBound = maxBound
        | otherwise = pred x

data RPSMove = Rock | Paper | Scissors deriving (Show, Eq)

instance Enum RPSMove where
    toEnum 0 = Rock
    toEnum 1 = Paper
    toEnum 2 = Scissors
    fromEnum Rock = 0
    fromEnum Paper = 1
    fromEnum Scissors = 2

instance Bounded RPSMove where
    minBound = Rock
    maxBound = Scissors

instance Circ RPSMove

instance Ord RPSMove where
    compare a b
        | b == next a = LT
        | b == prev a = GT
        | otherwise = EQ

type RPSRound = (RPSMove, RPSMove)

parseInput :: [String] -> [RPSRound]
parseInput = map parseLine
parseLine [theirMoveCh, ' ', roundResult] = (theirMove, myMove)
    where myMove 
              | roundResult == 'X' = prev theirMove
              | roundResult == 'Y' = theirMove
              | roundResult == 'Z' = next theirMove
          theirMove
              | theirMoveCh == 'A' = Rock
              | theirMoveCh == 'B' = Paper
              | theirMoveCh == 'C' = Scissors

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
