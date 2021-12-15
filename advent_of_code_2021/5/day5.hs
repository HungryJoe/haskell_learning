import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment (getArgs)


main = do
    args <- getArgs
    file <- readFile (head args)
    let parsedFile = parseFile file
    let freqMap1 = generateFreqMap $ filterOutDiagonals parsedFile
    let solution1 = countIntersections freqMap1
    let freqMap2 = generateFreqMap parsedFile
    let solution2 = countIntersections freqMap2
    print solution1
    print solution2


type FrequencyMap = Map.Map Point Int
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

generateFreqMap :: [Vector] -> FrequencyMap
generateFreqMap = fillMap (Set.empty :: Set.Set Point, Map.empty :: FrequencyMap)
    where fillMap :: (Set.Set Point, FrequencyMap) -> [Vector] -> FrequencyMap
          fillMap (_, freqMap) [] = freqMap
          fillMap (keySet, freqMap) ((Point{x=x1,y=y1}, Point{x=x2,y=y2}):vectors') = fillMap (addPoints lineSegment keySet freqMap) vectors'
              where listXs = if x1 <= x2 then [x1..x2] else [x1, (x1 - 1)..x2]
                    listYs = if y1 <= y2 then [y1..y2] else [y1, (y1 - 1)..y2]
                    lineSegment = map makePoint lineSegmentTups
                    lineSegmentTups
                        | x1 == x2 = map (x1 `entuple`) listYs
                        | y1 == y2 = map (`entuple` y1) listXs
                        | length listXs == length listYs = zip listXs listYs  -- Assume slope of 1
                        | otherwise = error $ "Invalid vector: " ++ show (x1,y1) ++ " " ++ show (x2, y2)
                    entuple a b = (a,b)
          addPoints :: [Point] -> Set.Set Point -> FrequencyMap -> (Set.Set Point, FrequencyMap)
          addPoints [] keySet freqMap = (keySet, freqMap)
          addPoints (point:points) keySet freqMap
            | Set.member point keySet = addPoints points keySet $ Map.update (Just . (+1)) point freqMap
            | otherwise = addPoints points (Set.insert point keySet) $ Map.insert point 1 freqMap

countIntersections :: FrequencyMap -> Int
countIntersections freqMap = length $ Map.filter (>= 2) freqMap
