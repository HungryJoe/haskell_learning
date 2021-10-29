import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import System.Environment

main = do 
    args <- getArgs
    let fileName = head args
    fileContentsText <- TextIO.readFile fileName
    fileContentsByteString <- ByteString.readFile $ head args
    let numLines = length $ Text.lines fileContentsText
    let numWords = length $ Text.words fileContentsText
    let numBytes = ByteString.length fileContentsByteString
    putStrLn $ show numLines ++ "\t" ++ show numWords ++ "\t" ++ show numBytes ++ "\t" ++ fileName
