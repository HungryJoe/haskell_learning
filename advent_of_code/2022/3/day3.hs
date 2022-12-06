import Data.Char (isLower, isUpper)
import System.Environment (getArgs)
import Data.Set (Set, fromList, intersection, union)
import Data.Foldable (fold, Foldable (toList))
import Control.Monad.List (foldM)
import Data.List
import qualified Data.Sequence (chunksOf, Seq, fromList)

type Rucksack = (Set Int, Set Int)

newtype SetIntersect a = SetIntersect {getSet :: Set a}
instance (Ord a) => Semigroup (SetIntersect a) where
    (<>) :: SetIntersect a -> SetIntersect a -> SetIntersect a
    (<>) x y = SetIntersect $ getSet x `intersection` getSet y

instance (Ord a) => Monoid (SetIntersect a) where
    mempty = SetIntersect $ fromList []

main = do
    args <- getArgs
    let fileName = head args
    fileContents <- readFile fileName
    let rucksacks = map parseLine $ lines fileContents
    let commonPriorities = findCommonPriorities rucksacks
    let commonPriorities3 = findCommonPrioritiesN 3 rucksacks
    print $ sum commonPriorities
    print $ sum commonPriorities3

charToPriority :: Char -> Int
charToPriority a
    | isLower a = fromEnum a - fromEnum 'a' + 1
    | isUpper a = fromEnum a - fromEnum 'A' + 27

parseLine :: String -> Rucksack
parseLine line = (fromList listA, fromList listB)
    where (listA, listB) = splitAt (div (length line) 2) $ map charToPriority line

findCommonPriority :: Rucksack -> Int
findCommonPriority (as, bs) = head $ toList $ intersection as bs

findCommonPriorities :: [Rucksack] -> [Int]
findCommonPriorities = map findCommonPriority

findCommonPriorityN :: [Rucksack] -> Int
findCommonPriorityN sacks = head $ toList $ getSet $ foldMap (SetIntersect . fold) sacks

findCommonPrioritiesN :: Int -> [Rucksack] -> [Int]
findCommonPrioritiesN n sacks = map findCommonPriorityN $ nestedSeqToNestedList $ Data.Sequence.chunksOf n sacksSeq
    where nestedSeqToNestedList = toList . fmap toList
          sacksSeq = Data.Sequence.fromList sacks
