import Data.Tree
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.String (IsString(fromString))

data File = File{size :: Int, name :: String}
data FileSystem = FileSystem{root :: Tree File, currDir :: [File]}
type CommandBlock = [String]

splitList :: (a -> Bool) -> [a] -> [[a]]
splitList pred (x:xs)
    | pred x = [x]:r:ecur
    | otherwise = (x:r):ecur
    where (r:ecur) = splitList pred xs

parseFile :: [String] -> Tree File
parseFile lines' = root $ foldl treeFolder FileSystem{root=rootNode, currDir=[rootLabel rootNode]} commands
    where (rootCmd:commands) = map (map $ drop 2) $ splitList ((=='$') . head) lines'
          rootNode = Node{rootLabel=File{size=0,name="/"}, subForest=[]}
          treeFolder :: FileSystem -> CommandBlock -> FileSystem
          treeFolder fs ("ls":contents) = fs{root=updateCurrDirTree fs $ parseLS (getCurrDirTree fs) contents}
          -- TODO: Figure out how to change roots while keeping track of the whole tree
          treeFolder fs ['c':'d':' ':arg] = fs{currDir=parseCD fs arg}

parseLS :: Tree File -> [String] -> Tree File
parseLS dir entries = Node {rootLabel=rootLabel dir, subForest=subForest dir ++ map (createNodeFromFile . parseEntry) entries}
    where createNodeFromFile f = Node {rootLabel=f, subForest=[]}

parseEntry :: String -> File
parseEntry line = File {size=size, name=name}
    where (firstHalf, secondHalf) = splitAt (fromJust $ elemIndex ' ' line) line
          (size, name)
            | firstHalf == "dir" = (0, secondHalf)
            | otherwise = (read firstHalf, secondHalf)

parseCD :: FileSystem -> String -> [File]

updateCurrDirrTree :: FileSystem -> Tree File -> Tree File

getcurrDirTree :: FileSystem -> Tree File
