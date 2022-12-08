import qualified Data.Text as T
import Data.Text.Internal.Builder (fromString)
import Data.List (elemIndex, stripPrefix)
import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)

data Move = Move {count :: Int, src :: Int, dest :: Int} deriving Show
type CrateStack = String


main = do
    args <- getArgs
    let fileName = head args
    fileContents <- readFile fileName
    let lines' = lines fileContents
    let (initialStacks, moves) = parseFile lines'
    let finalStacks1 = foldl executeMove1 initialStacks moves
    let finalStacks2 = foldl executeMove2 initialStacks moves
    print $ calculateScore finalStacks1
    print $ calculateScore finalStacks2

push :: CrateStack -> Char -> CrateStack
push xs x = x:xs

pop :: CrateStack -> (Char, CrateStack)
pop xs = (head xs, tail xs)

parseFile :: [String] -> ([CrateStack], [Move])
parseFile lines' = (parseCrateStacks $ takeWhile (not . isBtmCrateLine) crates, map parseMove $ drop 1 moves)
    where (crates, moves) = splitAt (fromJust $ elemIndex "" lines') lines'
          isBtmCrateLine (' ':'1':' ':rest) = True
          isBtmCrateLine _ = False

parseCrateStacks :: [String] -> [CrateStack]
parseCrateStacks [line] = map (:[]) $ parseCrateRow line
parseCrateStacks (line:rest) = zipWith pushNoBlank (parseCrateRow line) $ parseCrateStacks rest
    where pushNoBlank ' ' xs = xs
          pushNoBlank x xs = x:xs

--  Transform a line into a row of crates, *with blanks*, one entry per crate stack
--  "[Z] [A]    " -> ['Z','A',' ']
--  "[Z] [A] [B]" -> ['Z','A','B']
--  "        [B]" -> [' ',' ','B']
parseCrateRow :: String -> String
parseCrateRow (_:x:_:' ':rest) = x : parseCrateRow rest
parseCrateRow [_,x,_] = [x]

parseMove :: String -> Move
parseMove line = Move {count=count, src=src, dest=dest}
    where (count, rest) = parsePart "move " ' ' line
          (src, rest') = parsePart " from " ' ' rest
          (dest, _) = parsePart " to " ' ' rest'

-- "move 1 ..." -> "move " -> ' ' -> (1, "...")
-- move 3 from 8 to 2
parsePart :: String -> Char -> String -> (Int, String)
parsePart preStr delim line = (read part, rest)
    where (part, rest) 
            | isJust index = splitAt (fromJust index) trimmedLine
            | otherwise = (trimmedLine, "")
          index = elemIndex delim trimmedLine
          trimmedLine =  fromJust $ stripPrefix preStr line

executeMove1 :: [CrateStack] -> Move -> [CrateStack]
executeMove1 stacks Move {count=0, src=_, dest=_} = stacks
executeMove1 stacks Move {count=_count, src=_src, dest=_dest} = executeMove1 newSrcDestStacks Move {count=_count - 1, src=_src, dest=_dest}
    where (popped, newSrcStack) = pop $ stacks !! (_src - 1)
          newDestStack = push (stacks !! (_dest - 1)) popped
          (preSrcStacks, srcPlusPostStacks) = splitAt (_src - 1) stacks
          newSrcStacks = preSrcStacks ++ [newSrcStack] ++ drop 1 srcPlusPostStacks
          (preDestStacks, destPlusPostStacks) = splitAt (_dest - 1) newSrcStacks
          newSrcDestStacks = preDestStacks ++ [newDestStack] ++ drop 1 destPlusPostStacks

executeMove2 :: [CrateStack] -> Move -> [CrateStack]
executeMove2 stacks Move {count=_count, src=_src, dest=_dest} = newSrcDestStacks
    where (popped, newSrcStack) = splitAt _count $ stacks !! (_src - 1)
          newDestStack = popped ++ (stacks !! (_dest - 1))
          (preSrcStacks, srcPlusPostStacks) = splitAt (_src - 1) stacks
          newSrcStacks = preSrcStacks ++ [newSrcStack] ++ drop 1 srcPlusPostStacks
          (preDestStacks, destPlusPostStacks) = splitAt (_dest - 1) newSrcStacks
          newSrcDestStacks = preDestStacks ++ [newDestStack] ++ drop 1 destPlusPostStacks

calculateScore :: [CrateStack] -> String
calculateScore = map head
