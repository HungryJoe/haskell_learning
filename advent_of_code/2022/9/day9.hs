import Data.Set (Set, fromList, insert, empty)
import Control.Monad.Writer (Writer, mapWriter, runWriter, MonadWriter (writer, tell), foldM)

data Direction = DUp | DDown | DLeft | DRight deriving (Eq, Show)
data Move = Move{dir :: Direction, mag :: Int} deriving (Eq, Show)
data Point = Point{x :: Int, y :: Int} deriving (Eq, Ord, Show)
data Rope = Rope{head' :: Point, tail' :: Point} deriving (Show)

parseFile :: [String] -> [Move]
parseFile = map parseLine

parseLine :: String -> Move
parseLine (d:' ':m)
    | d == 'U' = Move{dir=DUp, mag=read m}
    | d == 'L' = Move{dir=DLeft, mag=read m}
    | d == 'R' = Move{dir=DRight, mag=read m}
    | d == 'D' = Move{dir=DDown, mag=read m}

executeMoves :: [Move] -> Writer (Set Point) Rope
executeMoves = foldM executeMove origin

origin :: Rope
origin = Rope{head'=Point{x=0,y=0}, tail'=Point{x=0,y=0}}

executeMove :: Rope -> Move -> Writer (Set Point) Rope
executeMove r Move{dir=_, mag=0} = writer (r, fromList [tail' r])
executeMove Rope {head'=_head', tail'=_tail'} m@Move{dir=_dir, mag=_mag} = mapWriter addTail $ executeMove Rope{head'=newHead, tail'=_tail' `moveTowards` newHead} m{mag=_mag - 1}
    where newHead
            | _dir == DUp = _head'{y=y _head' + 1}
            | _dir == DLeft = _head'{x=x _head' - 1}
            | _dir == DRight = _head'{x=x _head' + 1}
            | _dir == DDown = _head'{y=y _head' - 1}

addTail :: (Rope, Set Point) -> (Rope, Set Point)
addTail (r'@Rope {tail'=_tail'}, tailSet) = (r', insert _tail' tailSet)

moveTowards :: Point -> Point -> Point
moveTowards oldTail@Point {x=oTX, y=oTY} newHead@Point {x=nHX, y=nHY}
    | abs (oTX - nHX) <= 1 && abs (oTY - nHY) <= 1 = oldTail
    | oTX == nHX = oldTail{y=oTY `moveTowards1D` nHY}
    | oTY == nHY = oldTail{x=oTX `moveTowards1D` nHX}
    | otherwise = Point{x=oTX `moveTowards1D` nHX, y=oTY `moveTowards1D` nHY}
    where moveTowards1D toShift towards = toShift + signum (towards - toShift)

-- calculateScore :: Set Point -> Int
