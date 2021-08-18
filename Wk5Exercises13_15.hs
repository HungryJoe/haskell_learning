module Wk5Exercises13_15 where
import Wk4Exercises11_12

-- 13
testEncodeDirect :: Bool
testEncodeDirect = 
    encodeDirect "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
    && encodeDirect [1,1,1,3,4,4,1] == [Multiple 3 1,Single 3,Multiple 2 4, Single 1]
    && null (encodeDirect ([] :: [Int]))
encodeDirect :: (Eq a) => [a] -> [Wk4Exercises11_12.EncodedElement a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x : xs) =
    case head encXs of
        (Single y) -> if x == y then Multiple 2 x : tail encXs else Single x : encXs
        (Multiple i y) -> if x == y then Multiple (i + 1) x : tail encXs else Single x : encXs
    where
        encXs = encodeDirect xs

-- 14
testDuplicate :: Bool
testDuplicate =
    duplicate "kll" == "kkllll"
    && duplicate [1,2,3] == [1,1,2,2,3,3]
    && null (duplicate [])
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x : xs) = x : x : duplicate xs

-- 15
testRepli :: Bool
testRepli =
    repli "dll" 3 == "dddllllll"
    && repli [1] 1 == [1]
    && null (repli [] 100)
repli :: [a] -> Int -> [a]
repli [] i = []
repli (x : xs) i =
    let repliX 1 = [x]
        repliX n = x : repliX (n - 1)
    in
    repliX i ++ repli xs i
