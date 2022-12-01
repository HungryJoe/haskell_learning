import Data.List
import Data.Char (digitToInt)
import System.Environment (getArgs)


main = do
    args <- getArgs
    file <- readFile (head args)
    let parsedFile = determineAdjacents $ parseFile file
    let lowPoints = findLowPoints parsedFile
    let solution1 = calculateScore lowPoints
    print lowPoints
    print solution1


data Point = Point{x :: Int, y :: Int, z :: Int} deriving (Eq, Ord)
instance Show Point where
    show Point{x=x, y=y, z=z} = show (x, y, z)
data Adjacencies = Adjacencies{center :: Point, side1 :: Maybe Point, side2 :: Maybe Point, side3 :: Maybe Point, side4 :: Maybe Point}
instance Show Adjacencies where
    show Adjacencies{center=c, side1=s1, side2=s2, side3=s3, side4=s4} = show (c, s1, s2, s3, s4)
type Matrix = [[Point]]

parseFile :: String -> Matrix
parseFile file = zipWith parseLine [0..] (lines file)
    where parseLine y line = zipWith (parsePoint y) [0..] line
          parsePoint y x chZ = Point{x = x, y = y, z = digitToInt chZ}

-- Must be a square matrix
determineAdjacents :: Matrix -> [Adjacencies]
determineAdjacents mat = map (uncurry5 Adjacencies) $ getAdjacents2d mat
    where uncurry5 fn (a, b, c, d, e) = fn a b c d e

findLowPoints :: [Adjacencies] -> [Int]
findLowPoints [] = []
findLowPoints (adj:adjs)
    | isLowPoint adj = z (center adj) : findLowPoints adjs
    | otherwise = findLowPoints adjs
    where isLowPoint Adjacencies{center=c, side1=s1, side2=s2, side3=s3, side4=s4} = all (isNothingOrGT c) [s1, s2, s3, s4]
          isNothingOrGT p1 (Just p2) = z p2 > z p1
          isNothingOrGT _ Nothing = True

calculateScore :: [Int] -> Int
calculateScore lowPoints = length lowPoints + sum lowPoints

getAdjacents2d :: (Ord a, Eq a, Show a) => [[a]] -> [(a, Maybe a, Maybe a, Maybe a, Maybe a)]
getAdjacents2d = map combineAdjacents . groupAdjacents2d . findAdjacents2d
    where combineAdjacents [first@(a,b,c),second@(d,e,f)]
            | fstEq first second = (a, b, c, e, f)
            | otherwise = error $ "Expected " ++ show a ++ "==" ++ show d
          combineAdjacents xs = error $ "Expected exactly two tuples per group, found: " ++ show xs
          groupAdjacents2d = groupBy fstEq . sortOn fst3 . concat
          findAdjacents2d mat = map getAdjacents mat ++ map getAdjacents (transpose mat)
          fstEq (a, _, _) (b, _, _) = a == b
          fst3 (a, _, _) = a

getAdjacents :: [a] -> [(a, Maybe a, Maybe a)]
getAdjacents xs = zip3 xs ys (tail $ tail ys)
    where ys = Nothing : map Just xs ++ [Nothing]
