import Control.Monad.Writer (Writer, writer, mapWriter, foldM, runWriter)
import System.Environment (getArgs)

data CPU = CPU {cycle' :: Int, register :: Int} deriving Show
data Instruction = AddX Bool Int | Noop deriving Show

main = do
    args <- getArgs
    contents <- readFile $ head args
    let instructions = parseFile $ lines contents
    let result = executeInstructions instructions
    print $ calculateScore result

parseFile :: [String] -> [Instruction]
parseFile = map parseLine

parseLine :: String -> Instruction
parseLine str = case take 4 str of
    "addx"-> AddX False $ read $ drop 5 str
    "noop" -> Noop
    str -> error $ "Expected 'addx' or 'noop', got '" ++ str ++ "' instead"

executeInstructions :: [Instruction] -> Writer [CPU] CPU
executeInstructions = foldM executeInstruction initialCpu

initialCpu :: CPU
initialCpu = CPU {cycle'=1, register=1}

executeInstruction :: CPU -> Instruction -> Writer [CPU] CPU
executeInstruction cpu@CPU {cycle'=_cycle, register=_register} (AddX True n) = writer (CPU{cycle'=_cycle + 1, register=_register + n}, [cpu])
executeInstruction cpu@CPU {cycle'=_cycle} (AddX False n) = mapWriter (appendCpu cpu) $ executeInstruction cpu{cycle'=_cycle + 1} (AddX True n)
    where appendCpu cpu'' (cpu', cpus) = (cpu', cpu'' : cpus)
executeInstruction cpu@CPU {cycle'=_cycle} Noop = writer (cpu{cycle'=_cycle + 1}, [cpu])

calculateScore :: Writer [CPU] CPU -> Int
calculateScore w = sum [cycle' cpu * register cpu | cpu <- snd $ runWriter w, pickCycle cpu]
    where pickCycle CPU {cycle'=_cycle'} = (_cycle' + 20) `mod` 40 == 0
