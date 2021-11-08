import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import System.Environment

main = do
    args <- getArgs
    let Args{doWords=doWords, doLines=doLines, doBytes=doBytes, path=filePath} = parseArgs args
    fileContentsText <- TextIO.readFile filePath
    fileContentsByteString <- ByteString.readFile filePath
    putStrLn $ addLines doLines fileContentsText ++ addWords doWords fileContentsText ++ addBytes doBytes fileContentsByteString ++ filePath


data Args = Args{doWords :: Bool, doLines :: Bool, doBytes :: Bool, path :: FilePath}
parseArgs :: [String] -> Args
parseArgs args = allFalseToAllTrue $ foldl go Args{doWords=False, doLines=False, doBytes=False, path=""} args
    where go :: Args -> String -> Args
          go Args{doWords=words, doLines=lines, doBytes=bytes, path=path} arg = Args{
              doWords = words || parsedWords,
              doLines = lines || parsedLines,
              doBytes = bytes || parsedBytes,
              path = if null path then parsedPath else path
          }
              where Args{doWords=parsedWords, doLines=parsedLines, doBytes=parsedBytes, path=parsedPath} = parseArg arg
          parseArg ('-':flags) = parseFlags flags Args{doWords=False, doLines=False, doBytes=False, path=""}
          parseArg path = Args{doWords=False, doLines=False, doBytes=False, path=path}
          parseFlags [] args = args
          parseFlags ('w':flags) args = parseFlags flags args{doWords=True}
          parseFlags ('l':flags) args = parseFlags flags args{doLines=True}
          parseFlags ('c':flags) args = parseFlags flags args{doBytes=True}
          parseFlags (nonFlag:flags) args = error $ "Illegal option -- " ++ [nonFlag]
          -- Preserve the utility of OR as the folding function for the flags while making default behavior match `wc`
          allFalseToAllTrue args@Args{doWords=words, doLines=lines, doBytes=bytes, path=path}
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
    
