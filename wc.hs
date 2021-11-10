import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Text.Encoding
import qualified Data.ByteString as ByteString
import System.IO.Error
import System.Environment

main = do
    args <- getArgs
    let Args{doWords=doWords, doLines=doLines, doBytes=doBytes, paths=filePaths} = parseArgs args
    fileContentsBS <- mapM ByteString.readFile filePaths
    let fileContentsText = map decodeUtf8 fileContentsBS
    mapM_ (summarizeFile doLines doWords doBytes) (zip3 fileContentsBS fileContentsText filePaths)
    if length filePaths > 1 then do
        putStrLn $ summarizeAll doLines doWords doBytes fileContentsBS fileContentsText
    else do
        putStr ""


summarizeAll :: Bool -> Bool -> Bool -> [ByteString.ByteString] -> [Text.Text] -> String
summarizeAll doLines doWords doBytes fileContentsBS fileContentsText
    | doLines = show (sum $ map computeLines fileContentsText) ++ "\t" ++ summarizeAll False doWords doBytes fileContentsBS fileContentsText
    | doWords = show (sum $ map computeWords fileContentsText) ++ "\t" ++ summarizeAll False False doBytes fileContentsBS fileContentsText
    | doBytes = show (sum $ map computeBytes fileContentsBS) ++ "\t" ++ summarizeAll False False False fileContentsBS fileContentsText
    | otherwise = "total"

summarizeFile :: Bool -> Bool -> Bool -> (ByteString.ByteString, Text.Text, FilePath) -> IO ()
summarizeFile doLines doWords doBytes (fileContentsBS, fileContentsText, filePath) = do
    putStrLn $ formatStatText doLines fileContentsText computeLines ++
               formatStatText doWords fileContentsText computeWords ++
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

computeLines :: Text.Text -> Int
computeLines = length . Text.lines
    
computeWords :: Text.Text -> Int
computeWords = length . Text.words
    
computeBytes :: ByteString.ByteString -> Int
computeBytes = ByteString.length

formatStatBS :: Bool -> ByteString.ByteString -> (ByteString.ByteString -> Int) -> String
formatStatBS flag contents summarize
    | flag = show (summarize contents) ++ "\t"
    | otherwise = ""

formatStatText :: Bool -> Text.Text -> (Text.Text -> Int) -> String
formatStatText flag contents summarize
    | flag = show (summarize contents) ++ "\t"
    | otherwise = ""
