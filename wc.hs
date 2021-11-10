import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as Char8
import System.IO.Error
import System.Environment

main = do
    args <- getArgs
    let Args{doWords=doWords, doLines=doLines, doBytes=doBytes, paths=filePaths} = parseArgs args
    fileContentsBS <- mapM Char8.readFile filePaths
    mapM_ (summarizeFile doLines doWords doBytes) (zip fileContentsBS filePaths)
    if length filePaths > 1 then do
        putStrLn $ summarizeAll doLines doWords doBytes fileContentsBS
    else do
        putStr ""


summarizeAll :: Bool -> Bool -> Bool -> [Char8.ByteString] -> String
summarizeAll doLines doWords doBytes fileContentsBS
    | doLines = show (sum $ map computeLines fileContentsBS) ++ "\t" ++ summarizeAll False doWords doBytes fileContentsBS
    | doWords = show (sum $ map computeWords fileContentsBS) ++ "\t" ++ summarizeAll False False doBytes fileContentsBS 
    | doBytes = show (sum $ map computeBytes fileContentsBS) ++ "\t" ++ summarizeAll False False False fileContentsBS
    | otherwise = "total"

summarizeFile :: Bool -> Bool -> Bool -> (Char8.ByteString, FilePath) -> IO ()
summarizeFile doLines doWords doBytes (fileContentsBS, filePath) = do
    putStrLn $ formatStatBS doLines fileContentsBS computeLines ++
               formatStatBS doWords fileContentsBS computeWords ++
               formatStatBS doBytes fileContentsBS computeBytes ++
               filePath

data Args = Args{doWords :: Bool, doLines :: Bool, doBytes :: Bool, paths :: [FilePath]}
parseArgs :: [String] -> Args
parseArgs args = allFalseToAllTrue $ foldl go Args{doWords=False, doLines=False, doBytes=False, paths=[]} args
    where go :: Args -> String -> Args
          go Args{doWords=words, doLines=lines, doBytes=bytes, paths=paths} arg = Args{
              doWords = words || parsedWords,
              doLines = lines || parsedLines,
              doBytes = bytes || parsedBytes,
              paths = paths ++ parsedPath
          }
              where Args{doWords=parsedWords, doLines=parsedLines, doBytes=parsedBytes, paths=parsedPath} = parseArg arg
          parseArg ('-':flags) = parseFlags flags Args{doWords=False, doLines=False, doBytes=False, paths=[]}
          parseArg path = Args{doWords=False, doLines=False, doBytes=False, paths=[path]}
          parseFlags [] args = args
          parseFlags ('w':flags) args = parseFlags flags args{doWords=True}
          parseFlags ('l':flags) args = parseFlags flags args{doLines=True}
          parseFlags ('c':flags) args = parseFlags flags args{doBytes=True}
          parseFlags (nonFlag:flags) args = error $ "Illegal option -- " ++ [nonFlag]
          -- Preserve the utility of OR as the folding function for the flags while making default behavior match `wc`
          allFalseToAllTrue args@Args{doWords=words, doLines=lines, doBytes=bytes, paths=_}
            | not words && not lines && not bytes = args{doWords=True, doLines=True, doBytes=True}
            | otherwise = args

computeLines :: Char8.ByteString -> Int
computeLines = length . Char8.lines
    
computeWords :: Char8.ByteString -> Int
computeWords = length . Char8.words
    
computeBytes :: Char8.ByteString -> Int
computeBytes = Char8.length

formatStatBS :: Bool -> Char8.ByteString -> (Char8.ByteString -> Int) -> String
formatStatBS flag contents summarize
    | flag = show (summarize contents) ++ "\t"
    | otherwise = ""
