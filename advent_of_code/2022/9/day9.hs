import Data.Set (Set, fromList, insert, empty, elems, size, singleton)
import Control.Monad.Writer (Writer, mapWriter, runWriter, MonadWriter (writer, tell), foldM)
import System.Environment (getArgs)

data Direction = DUp | DDown | DLeft | DRight deriving (Eq, Show)
data Move = Move{dir :: Direction, mag :: Int} deriving (Eq, Show)
data Point = Point{x :: Int, y :: Int} deriving (Eq, Ord, Show)
type Rope = [Point]

main = do
    args <- getArgs
    fileContents <- readFile $ head args
    let moves = parseFile $ lines fileContents
    let result1 = executeMoves 2 moves
    let result2 = executeMoves 10 moves
    print $ calculateScore result1
    print $ calculateScore result2

parseFile :: [String] -> [Move]
parseFile = map parseLine

parseLine :: String -> Move
parseLine (d:' ':m)
    | d == 'U' = Move{dir=DUp, mag=read m}
    | d == 'L' = Move{dir=DLeft, mag=read m}
    | d == 'R' = Move{dir=DRight, mag=read m}
    | d == 'D' = Move{dir=DDown, mag=read m}

executeMoves :: Int -> [Move] -> Writer (Set Point) Rope
executeMoves n = foldM executeMove (originN n)

originN :: Int -> Rope
originN n = replicate n Point {x=0, y=0}

executeMove :: Rope -> Move -> Writer (Set Point) Rope
executeMove r Move{dir=_, mag=0} = writer (r, singleton $ last r)
executeMove (head':rest) m@Move{dir=_dir, mag=_mag} = mapWriter (addTail $ last rest) $ executeMove newRope m{mag=_mag - 1}
    where newHead
            | _dir == DUp = head'{y=y head' + 1}
            | _dir == DLeft = head'{x=x head' - 1}
            | _dir == DRight = head'{x=x head' + 1}
            | _dir == DDown = head'{y=y head' - 1}
          newRope = scanl (flip moveTowards) newHead rest

addTail :: Point -> (Rope, Set Point) -> (Rope, Set Point)
addTail newTail (r', tailSet) = (r', insert newTail tailSet)

moveTowards :: Point -> Point -> Point
moveTowards oldTail@Point {x=oTX, y=oTY} newHead@Point {x=nHX, y=nHY}
    | abs (oTX - nHX) <= 1 && abs (oTY - nHY) <= 1 = oldTail
    | oTX == nHX = oldTail{y=oTY `moveTowards1D` nHY}
    | oTY == nHY = oldTail{x=oTX `moveTowards1D` nHX}
    | otherwise = Point{x=oTX `moveTowards1D` nHX, y=oTY `moveTowards1D` nHY}
    where moveTowards1D toShift towards = toShift + signum (towards - toShift)

calculateScore :: Writer (Set Point) Rope -> Int
calculateScore = size . snd . runWriter
