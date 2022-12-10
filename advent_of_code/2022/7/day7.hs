import Data.Tree
import Data.List (elemIndex, find)
import Data.Maybe (fromJust)
import Data.String (IsString(fromString))

-- Assume that files of size 0 are directories
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
parseCD fs@FileSystem {currDir=_currDir} ".." = tail _currDir
-- Assume that a directory named `dir` exists in  `last _currDir`
parseCD fs@FileSystem {root=_root, currDir=_currDir} childDir = _currDir ++ [rootLabel childDirTree]
    where childDirTree = findTreeMatching File {size=0, name=childDir} $ subForest $ getCurrDirTree fs

getCurrDirTree :: FileSystem -> Tree File
getCurrDirTree FileSystem {root=_root, currDir=_currDir} = go _root _currDir
    where go currentRootTree@Node {rootLabel=_rootLabel, subForest=_subForest} (currentRoot:rest)
            | _rootLabel == currentRoot = currentRootTree
            | otherwise = go (findTreeMatching (head rest) _subForest) rest

findTreeMatching :: File -> [Tree File] -> Tree File
findTreeMatching file (node@Node {rootLabel=_rootLabel}:rest)
    | file == _rootLabel = node
    | otherwise = findTreeMatching file rest

-- needs to be fully recursive (i.e. no "go" function) so that we can build up the entire root file-tree
replaceCurrDirTreeInRoot :: FileSystem -> Tree File -> Tree File
replaceCurrDirTreeInRoot FileSystem {root=_root, currDir=_currDir} currDirTree = 
