import Data.List

data Point = Point{x :: Int, y :: Int, z :: Int} deriving (Ord, Eq)
instance Show Point where
    show Point{x=x, y=y, z=z} = show (x, y, z)
data Adjacencies = Adjacencies{center :: Point, side1 :: Point, side2 :: Point, side3 :: Point, side4 :: Point}
type Matrix = [[Point]]

-- parseFile :: String -> Matrix
-- 
-- -- Must be a square matrix
-- determineAdjacents :: Matrix -> [Adjacencies]
-- determineAdjacents 
-- 
-- findLowPoints :: [(Int, [Int])] -> [Int]

getAdjacents2d :: (Ord a, Eq a, Show a) => [[a]] -> [(Maybe a, Maybe a, Maybe a, Maybe a, Maybe a)]
getAdjacents2d = map combineAdjacents . groupAdjacents2d . findAdjacents2d
    where combineAdjacents [first@(a,b,c),second@(d,e,f)]
            | fstEq first second = (a, b, c, e, f)
            | otherwise = error $ "Expected " ++ show a ++ "==" ++ show d
          combineAdjacents xs = error $ "Expected exactly two tuples per group, found: " ++ show xs
          groupAdjacents2d = groupBy fstEq . sortOn fst3 . concat
          findAdjacents2d mat = map getAdjacents mat ++ map getAdjacents (transpose mat)
          fstEq (a, _, _) (b, _, _) = a == b
          fst3 (a, _, _) = a
    
getAdjacents :: [a] -> [(Maybe a, Maybe a, Maybe a)]
getAdjacents xs = zip3 (tail ys) ys (tail $ tail ys)
    where ys = Nothing : map Just xs ++ [Nothing]

