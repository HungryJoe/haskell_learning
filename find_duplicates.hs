import qualified Crypto.Hash.SHA256 as H
import qualified Data.ByteString as BS
import System.Directory
import Control.Monad
import qualified Data.Map.Strict as M

main = do
    workingDir <- getCurrentDirectory
    filePaths <- listFiles workingDir
    fileContents <- mapM BS.readFile filePaths
    let pathsAndHashes = [(path, H.hash contents) | (contents, path) <- zip fileContents filePaths]
    let results = findDuplicates pathsAndHashes
    putStrLn $ showDuplicates results


showDuplicates :: [[FilePath]] -> String
showDuplicates [] = "No duplicates"
showDuplicates lines = "Duplicates:\n" ++ concatMap showLine lines
    where showLine paths = concatMap showPath (init paths) ++ last paths ++ "\n"
          showPath path = path ++ ","

listFiles :: FilePath -> IO [FilePath]
listFiles path = do
    contents <- listDirectory path
    filterM doesFileExist contents

findDuplicates :: [(FilePath, BS.ByteString)] -> [[FilePath]]
findDuplicates = filter lenG1 . M.elems . makeHashMap
    where lenG1 xs = length xs > 1

makeHashMap :: [(FilePath, BS.ByteString)] -> M.Map BS.ByteString [FilePath]
makeHashMap [] = M.empty
makeHashMap ((path, hash'):theRest) = M.alter insertOrUpdate hash' $ makeHashMap theRest
    where insertOrUpdate (Just paths) = Just $ path : paths
          insertOrUpdate Nothing = Just [path]
