import qualified Data.Text as T

data Move = Move {count :: Int, src :: Int, dest :: Int}
type CrateStack = T.Text

push :: CrateStack -> Char -> CrateStack
push xs x = T.cons x xs

pop :: CrateStack -> (Char, CrateStack)
pop xs = (T.head xs, T.tail xs)

-- parseFile :: [String] -> ([CrateStack], [Move])

parseCrateStacks :: [String] -> [CrateStack]
parseCrateStacks [line] = map T.singleton $ parseCrateRow line
parseCrateStacks (line:rest) = zipWith pushNoBlank (parseCrateRow line) $ parseCrateStacks rest
    where pushNoBlank ' ' xs = xs
          pushNoBlank x xs = T.cons x xs

--  Transform a line into a row of crates, *with blanks*, one entry per crate stack
--  "[Z] [A]    " -> ['Z','A',' ']
--  "[Z] [A] [B]" -> ['Z','A','B']
--  "        [B]" -> [' ',' ','B']
parseCrateRow :: String -> String
parseCrateRow (_:x:_:' ':rest) = x : parseCrateRow rest
parseCrateRow [_,x,_] = [x]

-- parseMove :: String -> Move

-- executeMove :: [CrateStack] -> Move -> [CrateStack]
