import Data.Tree
import Data.List (elemIndex, find, findIndex)
import Data.Maybe (fromJust, isJust)
import Data.String (IsString(fromString))
import Data.Sequence (chunksOf)

-- Assume that files of size 0 are directories
data File = File{size :: Int, name :: String} deriving (Eq, Show)
data FileSystem = FileSystem{root :: Tree File, currDir :: [File]}
type CommandBlock = [String]

chunkAroundPred :: (a -> Bool) -> [a] -> [[a]]
chunkAroundPred _ [] = []
chunkAroundPred pred (x:xs)
    | not (null recur) && pred (head $ head recur) = [x] : recur
    | not (null recur) = (x : head recur) : tail recur
    | otherwise = [x] : recur
    where recur = chunkAroundPred pred xs

parseFile :: [String] -> Tree File
parseFile lines' = root $ foldl treeFolder FileSystem{root=rootNode, currDir=[rootLabel rootNode]} commands
    where (rootCmd:commands) = map (map $ drop 2) $ chunkAroundPred ((=='$') . head) lines'
          rootNode = Node{rootLabel=File{size=0,name="/"}, subForest=[]}
          treeFolder :: FileSystem -> CommandBlock -> FileSystem
          treeFolder fs ("ls":contents) = fs{root=replaceCurrDirTreeInRoot fs $ parseLS (getCurrDirTree fs) contents}
          treeFolder fs ['c':'d':' ':arg] = fs{currDir=parseCD fs arg}
          treeFolder fs cmd = error $ show cmd

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
parseCD _ "/" = [File {size=0, name="/"}]
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

replaceCurrDirTreeInRoot :: FileSystem -> Tree File -> Tree File
replaceCurrDirTreeInRoot FileSystem {root=Node {rootLabel=_rootLabel, subForest=_subForest}, currDir=(rootDir:childDir:dirs)} currDirTree
    | _rootLabel == rootDir = Node {rootLabel=_rootLabel, subForest=replace childDirTree recur _subForest}
    where recur =  replaceCurrDirTreeInRoot FileSystem {root=childDirTree, currDir=childDir:dirs} currDirTree
          childDirTree = findTreeMatching childDir _subForest
replaceCurrDirTreeInRoot FileSystem {root=Node {rootLabel=_rootLabel, subForest=_subForest}, currDir=[rootDir]} currDirTree
    | _rootLabel == rootDir = currDirTree

replace :: Eq a => a -> a -> [a] -> [a]
replace toReplace replacement (x:xs)
    | toReplace == x = replacement:xs
    | otherwise = x : replace toReplace replacement xs
