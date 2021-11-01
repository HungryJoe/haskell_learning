import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import System.Environment

main = do
    args <- getArgs
    let Args{doWords=doWords, doLines=doLines, doBytes=doBytes, path=filePath} = parseArgs args
    fileContentsText <- TextIO.readFile filePath
    fileContentsByteString <- ByteString.readFile filePath
    putStrLn $ addLines doLines fileContentsText ++ "\t" ++ addWords doWords fileContentsText ++ "\t" ++ addBytes doBytes fileContentsByteString ++ "\t" ++ filePath


data Args = Args{doWords :: Bool, doLines :: Bool, doBytes :: Bool, path :: FilePath}
parseArgs :: [String] -> Args
parseArgs = foldl go Args{doWords=True, doLines=True, doBytes=True, path=""}
    where go :: Args -> String -> Args
          go Args{doWords=words, doLines=lines, doBytes=doBytes, path=path} arg = Args{
              doWords = words && parsedWords,
              doLines = lines && parsedLines,
              doBytes = doBytes && parsedBytes,
              path = if null path then parsedPath else path
          }
              where Args{doWords=parsedWords, doLines=parsedLines, doBytes=parsedBytes, path=parsedPath} = parseArg arg
          parseArg "-w" = Args{doWords=True, doLines=False, doBytes=False, path=""}
          parseArg "-l" = Args{doWords=False, doLines=True, doBytes=False, path=""}
          parseArg "-c" = Args{doWords=False, doLines=False, doBytes=True, path=""}
          parseArg path = Args{doWords=True, doLines=True, doBytes=True, path=path}

addLines :: Bool -> Text.Text -> String
addLines flag contents
    | flag = show $ length $ Text.lines contents
    | otherwise = ""
    
addWords :: Bool -> Text.Text -> String
addWords flag contents
    | flag = show $ length $ Text.words contents
    | otherwise = ""
    
addBytes :: Bool -> ByteString.ByteString -> String
addBytes flag contents
    | flag = show $ ByteString.length contents
    
    | otherwise = ""
    
