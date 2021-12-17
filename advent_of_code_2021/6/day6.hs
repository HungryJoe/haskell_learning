import System.Environment
import qualified Data.Map as M
import Data.List (sort)


main = do
    args <- getArgs
    file <- readFile $ head args
    let solution1 = M.foldl (+) 0 $ simulateFishForNDays (parseFile file) (read (last args) :: Int)
    -- let solution2 = length $ simulateFishForNDays (parseFile file) 256
    print solution1
    -- print solution2

type Population = M.Map Fish Int
newtype Fish = Fish Int deriving (Ord, Eq)

parseFile :: String -> Population
parseFile file = foldl makeFreqMap (M.empty :: Population) (map Fish (read ('[' : file ++ "]") :: [Int]))
    where makeFreqMap freqMap fish
            | M.member fish freqMap = M.update (Just . (+1)) fish freqMap
            | otherwise = M.insert fish 1 freqMap

simulateFishForNDays :: Population -> Int -> Population
simulateFishForNDays fishes 0 = fishes
simulateFishForNDays fishes n = simulateFishForNDays (simulateFishOneDay fishes) (n - 1)

simulateFishOneDay :: Population -> Population
simulateFishOneDay = M.foldlWithKey' stateTransition (M.empty :: Population)
    where stateTransition newFishes (Fish 0) pop = M.alter (addTo pop) (Fish 8) $ M.alter (addTo pop) (Fish 6) newFishes
          stateTransition newFishes (Fish fish) pop = M.alter (addTo pop) (Fish $ fish - 1) newFishes
          addTo :: Int -> Maybe Int -> Maybe Int
          addTo new (Just old) = Just (old + new)
          addTo new Nothing = Just new
          zeroOut (Just _) = Just 0
          zeroOut Nothing = Nothing
