import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment (getArgs)


main = do
    args <- getArgs
    file <- readFile (head args)
    let parsedFile = filterOutDiagonals $ parseFile file
    let freqMap = generateFreqMap parsedFile
    let solution = countIntersections freqMap
    -- print freqMap
    print solution


type Vector = (Point, Point)
data Point = Point{x :: Int, y :: Int} deriving (Eq, Ord)
instance Show Point where
    show Point{x=x, y=y} = '(' : show x ++ ", " ++ show y ++ ")"
makePoint :: (Int, Int) -> Point
makePoint (x, y) = Point{x=x, y=y}

parseFile :: String -> [Vector]
parseFile file = map parseLine $ lines file
    where parseLine line = (readPoint startStr, readPoint endStr)
            where (startStr, endStr) = (init $ takeWhile (/= '-') line, tail $ tail $ dropWhile (/= '>') line)
                  readPoint str = Point{x = getX str, y = getY str}
                  getX str = (read $ takeWhile (/= ',') str) :: Int
                  getY str = (read $ tail $ dropWhile (/= ',') str) :: Int

filterOutDiagonals :: [Vector] -> [Vector]
filterOutDiagonals vectors = [vector | vector@(Point{x=x1, y=y1}, Point{x=x2, y=y2}) <- vectors, x1 == x2 || y1 == y2]

generateFreqMap :: [Vector] -> Map.Map Point Int
generateFreqMap = fillMap (Map.empty :: Map.Map Point Int)
    where fillMap freqMap [] = freqMap
          fillMap freqMap ((Point{x=x1,y=y1}, Point{x=x2,y=y2}):vectors') = fillMap (addPoints (Set.toList product) freqMap) vectors'
              where product = Set.map makePoint $ Set.cartesianProduct (Set.fromList listXs) (Set.fromList listYs)
                    listXs = if x1 < x2 then [x1..x2] else [x2..x1]
                    listYs = if y1 < y2 then [y1..y2] else [y2..y1]
          addPoints [] freqMap = freqMap
          addPoints (point:points) freqMap
            | Set.member point (Map.keysSet freqMap) = addPoints points $ Map.update (Just . (+1)) point freqMap
            | otherwise = addPoints points $ Map.insert point 1 freqMap

countIntersections :: Map.Map Point Int -> Int
countIntersections freqMap = length $ Map.filter (>= 2) freqMap
