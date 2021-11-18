import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as Char8
import System.IO.Error
import System.Environment

main = do
    args <- getArgs
    let Args{doWords=doWords, doLines=doLines, doChars=doChars, paths=filePaths} = parseArgs args
    if not $ null filePaths then do
        fileContentsBS <- mapM (flip catchIOError handleError . Char8.readFile) filePaths
        let fileContentsText = map decodeUtf8 fileContentsBS
        mapM_ (summarizeFile doLines doWords doChars) (zip3 fileContentsBS fileContentsText filePaths)
        if length filePaths > 1 then do
            putStrLn $ summarizeAll doLines doWords doChars fileContentsBS fileContentsText
        else do
            putStr ""
    else do
        stdinContentsBytes <- Char8.getContents
        let stdinContentsText = decodeUtf8 stdinContentsBytes
        summarizeFile doLines doWords doChars (stdinContentsBytes, stdinContentsText, "")


summarizeAll :: Bool -> Bool -> CharOpts -> [Char8.ByteString] -> [Text.Text] -> String
summarizeAll doLines doWords doChars fileContentsBS fileContentsText
    | doLines = show (sum $ map computeLines fileContentsBS) ++ "\t" ++ next False doWords doChars
    | doWords = show (sum $ map computeWords fileContentsBS) ++ "\t" ++ next False False doChars
    | doChars == Bytes = show (sum $ map computeBytes fileContentsBS) ++ "\t" ++ next False False NoChars
    | doChars == TextChars = show (sum $ map computeTextChars fileContentsText) ++ "\t" ++ next False False NoChars
    | otherwise = "total"
    where next doLines' doWords' doChars' = summarizeAll doLines' doWords' doChars' fileContentsBS fileContentsText

summarizeFile :: Bool -> Bool -> CharOpts -> (Char8.ByteString, Text.Text, FilePath) -> IO ()
summarizeFile doLines doWords doChars (fileContentsBS, fileContentsText, filePath) = do
    putStrLn $ formatStatBS doLines fileContentsBS computeLines ++
               formatStatBS doWords fileContentsBS computeWords ++
               formatStatBS (doChars == Bytes) fileContentsBS computeBytes ++
               formatStatText (doChars == TextChars) fileContentsText computeTextChars ++
               filePath

data CharOpts = TextChars | Bytes | NoChars deriving Eq
data Args = Args{doWords :: Bool, doLines :: Bool, doChars :: CharOpts, paths :: [FilePath]}
parseArgs :: [String] -> Args
parseArgs args = allFalseToAllTrue $ foldl go Args{doWords=False, doLines=False, doChars=NoChars, paths=[]} args
    where go :: Args -> String -> Args
          go Args{doWords=words, doLines=lines, doChars=chars, paths=paths} arg = Args{
              doWords = words || parsedWords,
              doLines = lines || parsedLines,
              doChars = if parsedChars == NoChars then chars else parsedChars,
              paths = paths ++ parsedPath
          }
              where Args{doWords=parsedWords, doLines=parsedLines, doChars=parsedChars, paths=parsedPath} = parseArg arg
          parseArg ('-':flags) = parseFlags flags Args{doWords=False, doLines=False, doChars=NoChars, paths=[]}
          parseArg path = Args{doWords=False, doLines=False, doChars=NoChars, paths=[path]}
          parseFlags [] args = args
          parseFlags ('w':flags) args = parseFlags flags args{doWords=True}
          parseFlags ('l':flags) args = parseFlags flags args{doLines=True}
          parseFlags ('c':flags) args = parseFlags flags args{doChars=Bytes}
          parseFlags ('m':flags) args = parseFlags flags args{doChars=TextChars}
          parseFlags (nonFlag:flags) args = error $ "Illegal option -- " ++ [nonFlag]
          -- Preserve the utility of OR as the folding function for the flags while making default behavior match `wc`
          allFalseToAllTrue args@Args{doWords=words, doLines=lines, doChars=chars, paths=_}
            | not words && not lines && chars == NoChars = args{doWords=True, doLines=True, doChars=Bytes}
            | otherwise = args

computeLines :: Char8.ByteString -> Int
computeLines = length . Char8.lines
    
computeWords :: Char8.ByteString -> Int
computeWords = length . Char8.words
    
computeBytes :: Char8.ByteString -> Int
computeBytes = Char8.length

computeTextChars :: Text.Text -> Int
computeTextChars = Text.length

formatStatBS :: Bool -> Char8.ByteString -> (Char8.ByteString -> Int) -> String
formatStatBS flag contents summarize
    | flag = show (summarize contents) ++ "\t"
    | otherwise = ""

formatStatText :: Bool -> Text.Text -> (Text.Text -> Int) -> String
formatStatText flag contents summarize
    | flag = show (summarize contents) ++ "\t"
    | otherwise = ""

handleError :: IOError -> IO Char8.ByteString
handleError err = do
    putStrLn $  fileName (ioeGetFileName err) ++ ioeGetErrorString err
    return $ Char8.pack ""
    where fileName Nothing = ""
          fileName (Just name) = name ++ " "
