import Data.Tree
import Data.List (elemIndex, find, findIndex, stripPrefix)
import Data.Maybe (fromJust, isJust)
import Data.String (IsString(fromString))
import Data.Sequence (chunksOf)
import qualified Data.List.NonEmpty as NE
import System.Environment (getArgs)

-- Assume that files of size 0 are directories
data File = File{size :: Int, name :: String} deriving (Eq, Show)
data FileSystem = FileSystem{root :: Tree File, currDir :: [File]} deriving Show
type CommandBlock = [String]

main = do
    args <- getArgs
    let fileName = head args
    fileContents <- readFile fileName
    let fileSystem = parseFile $ lines fileContents
    let score = calculateScore fileSystem
    print score

chunkAroundPred :: (a -> Bool) -> [a] -> [[a]]
chunkAroundPred _ [] = []
chunkAroundPred pred (x:xs)
    | not (null recur) && pred (head $ head recur) = [x] : recur
    | not (null recur) = (x : head recur) : tail recur
    | otherwise = [x] : recur
    where recur = chunkAroundPred pred xs

parseFile :: [String] -> FileSystem
parseFile lines' = foldl treeFolder FileSystem{root=rootNode, currDir=[rootLabel rootNode]} $ parseIntoCommands lines'
    where rootNode = Node{rootLabel=File{size=0,name="/"}, subForest=[]}

parseIntoCommands :: [String] -> [CommandBlock]
parseIntoCommands lines' = map mapper $ chunkAroundPred ((==commandStart) . take 2) lines'
    where mapper l = fromJust (stripPrefix commandStart $ head l) : tail l
          commandStart = "$ "

treeFolder :: FileSystem -> CommandBlock -> FileSystem
treeFolder fs ("ls":contents) = fs{root=replaceCurrDirTreeInRoot fs $ parseLS (getCurrDirTree fs) contents}
treeFolder fs ['c':'d':' ':arg] = fs{currDir=parseCD fs arg}
treeFolder fs cmd = error $ show cmd

parseLS :: Tree File -> [String] -> Tree File
parseLS dir entries = Node {rootLabel=rootLabel dir, subForest=subForest dir ++ map (createNodeFromFile . parseEntry) entries}
    where createNodeFromFile f = Node {rootLabel=f, subForest=[]}

parseEntry :: String -> File
parseEntry line = File {size=size, name=name}
    where (firstHalf, ' ':secondHalf) = splitAt (fromJust $ elemIndex ' ' line) line
          (size, name)
            | firstHalf == "dir" = (0, secondHalf)
            | otherwise = (read firstHalf, secondHalf)

parseCD :: FileSystem -> String -> [File]
parseCD fs@FileSystem {currDir=_currDir} ".." = init _currDir
parseCD _ "/" = [File {size=0, name="/"}]
-- Assume that a directory named `dir` exists in  `last _currDir`
parseCD fs@FileSystem {root=_root, currDir=_currDir} childDir = _currDir ++ [rootLabel childDirTree]
    where childDirTree = findTreeMatching File {size=0, name=childDir} $ subForest $ getCurrDirTree fs

getCurrDirTree :: FileSystem -> Tree File
getCurrDirTree FileSystem {root=_root, currDir=_currDir} = go _root _currDir
    where go Node {rootLabel=_rootLabel, subForest=_subForest} (rootDir:childDir:rest)
            | _rootLabel == rootDir = go (findTreeMatching childDir _subForest) (childDir:rest)
          go currRoot@Node {rootLabel=_rootLabel, subForest=_subForest} [rootDir]
            | _rootLabel == rootDir = currRoot

findTreeMatching :: File -> [Tree File] -> Tree File
findTreeMatching file (node@Node {rootLabel=_rootLabel}:rest)
    | file == _rootLabel = node
    | otherwise = findTreeMatching file rest
findTreeMatching file [] = error $ "Couldn't find: " ++ show file

replaceCurrDirTreeInRoot :: FileSystem -> Tree File -> Tree File
replaceCurrDirTreeInRoot FileSystem {root=node@Node {rootLabel=_rootLabel, subForest=_subForest}, currDir=(rootDir:childDir:dirs)} currDirTree
    | _rootLabel == rootDir = node{subForest=replace childDirTree recur _subForest}
    where recur =  replaceCurrDirTreeInRoot FileSystem {root=childDirTree, currDir=childDir:dirs} currDirTree
          childDirTree = findTreeMatching childDir _subForest
replaceCurrDirTreeInRoot FileSystem {root=Node {rootLabel=_rootLabel, subForest=_subForest}, currDir=[rootDir]} currDirTree
    | _rootLabel == rootDir = currDirTree

replace :: Eq a => a -> a -> [a] -> [a]
replace toReplace replacement (x:xs)
    | toReplace == x = replacement:xs
    | otherwise = x : replace toReplace replacement xs

calculateScore :: FileSystem -> Int
calculateScore FileSystem {root=_root, currDir=_} = sum $ filter (<=100000) $ flattenNoLeaves $ calculateDirectorySizes _root

flattenNoLeaves :: Tree a -> [a]
flattenNoLeaves Node {rootLabel=_, subForest=[]} = []
flattenNoLeaves Node {rootLabel=_rootLabel, subForest=_subForest} = _rootLabel : concatMap flattenNoLeaves _subForest

calculateDirectorySizes :: Tree File -> Tree Int
calculateDirectorySizes n@Node {rootLabel=file, subForest=[]} = Node {rootLabel=size file, subForest=[]}
calculateDirectorySizes n@Node {rootLabel=_rootLabel, subForest=_subForest} = Node {rootLabel=sum $ map rootLabel subForestSizes, subForest=subForestSizes}
    where subForestSizes = map calculateDirectorySizes _subForest
