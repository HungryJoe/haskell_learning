import Control.Monad.Writer (Writer, writer, mapWriter, foldM, runWriter, sequence, sequence_)
import System.Environment (getArgs)
import Data.Sequence (chunksOf, fromList)
import Data.Foldable (Foldable(toList))
import Data.List (sortOn)

data CPU = CPU {cycle' :: Int, register :: Int} deriving Show
data Instruction = AddX Bool Int | Noop deriving Show
type CRT = [[Bool]]  -- Assume 6 rows of 40 columns each

main = do
    args <- getArgs
    contents <- readFile $ head args
    let instructions = parseFile $ lines contents
    let result = executeInstructions instructions
    let crt = displayCpus $ snd $ runWriter result
    print $ calculateScore result
    printCRT crt

parseFile :: [String] -> [Instruction]
parseFile = map parseLine

parseLine :: String -> Instruction
parseLine str = case take 4 str of
    "addx"-> AddX False $ read $ drop 5 str
    "noop" -> Noop
    str -> error $ "Expected 'addx' or 'noop', got '" ++ str ++ "' instead"

rowLength :: Int
rowLength = 40

-- Assume that the input has length of 240 with cycle' covering the interval [1,240]
displayCpus :: [CPU] -> CRT
displayCpus cpus = listToDisplay $ map cpuToPixel $ sortOn cycle' cpus
    where listToDisplay l = toList $ fmap toList $ chunksOf rowLength $ fromList l
cpuToPixel CPU {cycle'=_cycle', register=_register} = pixelInRow >= (_register - 1) && pixelInRow <= (_register + 1)
    where pixel = _cycle' - 1
          pixelInRow = pixel `mod` rowLength

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

printCRT :: CRT -> IO [()]
printCRT = mapM (putStrLn . map boolToChar)
    where boolToChar False = '.'
          boolToChar True = '#'
