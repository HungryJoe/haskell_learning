import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Data.ByteString as ByteString
import System.IO.Error
import System.Environment

main = do
    args <- getArgs
    let Args{doWords=doWords, doLines=doLines, doBytes=doBytes, paths=filePaths} = parseArgs args
    mapM (processFile doLines doWords doBytes) filePaths


processFile :: Bool -> Bool -> Bool -> FilePath -> IO ()
processFile doLines doWords doBytes filePath = do
    fileContentsByteString <- ByteString.readFile filePath
    let fileContentsText = decodeUtf8 fileContentsByteString
    putStrLn $ addLines doLines fileContentsText ++
               addWords doWords fileContentsText ++
               addBytes doBytes fileContentsByteString ++
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

addLines :: Bool -> Text.Text -> String
addLines flag contents
    | flag = show (length $ Text.lines contents) ++ "\t"
    | otherwise = ""
    
addWords :: Bool -> Text.Text -> String
addWords flag contents
    | flag = show (length $ Text.words contents) ++ "\t"
    | otherwise = ""
    
addBytes :: Bool -> ByteString.ByteString -> String
addBytes flag contents
    | flag = show (ByteString.length contents) ++ "\t"
    | otherwise = ""
