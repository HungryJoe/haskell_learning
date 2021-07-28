-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs
testIsPalindrome :: Bool
testIsPalindrome =
    not (isPalindrome [1,2,3])
    && isPalindrome "madamimadam"
    && isPalindrome [1,2,4,8,16,8,4,2,1]
    && isPalindrome ([] :: [Integer])

-- 7
data NestedList a = Elem a | List [NestedList a]
testFlatten :: Bool
testFlatten =
    flatten (Elem 5) == [5]
    && flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1, 2, 3, 4, 5]
    && null (flatten (List []))
flatten :: NestedList a -> [a]
flatten xs = 
    case xs of
        List [] -> []
        Elem x -> [x]
        List (Elem x:xs) -> x : flatten (List xs)
        List (List x:xs) -> flatten (List (x ++ xs))

-- 8
testDeduplicate :: Bool
testDeduplicate =
    deduplicate "aaaabccaadeeee" == "abcade"
    && deduplicate [0, 1, 0, 0, 0, 0, 1, 1] == [0, 1, 0, 1]
    && null (deduplicate ([] :: [Integer]))
deduplicate :: (Eq a) => [a] -> [a]
deduplicate xs =
    case xs of
        [] -> []
        (x:xs) ->
            if not (null xs) && x == head xs then -- why doesn't this fail sometimes on the empty list?
                deduplicate xs
            else
                x : deduplicate xs

-- 9
testPack :: Bool
testPack =
    pack "aaaabccaadeeee" == ["aaaa","b","cc","aa","d","eeee"]
    && pack [1, 2, 3, 4] == [[1], [2], [3], [4]]
    && null (pack ([] :: [Integer]))
pack :: (Eq a) => [a] -> [[a]]
pack xs =
    case xs of
        [] -> []
        (x:xs) ->
            if not (null xs) && x == head (head (pack xs)) then
                (x : head (pack xs)) : tail (pack xs)
            else
                [x] : pack xs

-- 10
-- Run-length encoding
testEncode :: Bool
testEncode =
    encode "aaaabccaadeeee" == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
    && encode [1, 2, 3, 4] == [(1, 1), (1, 2), (1, 3), (1, 4)]
    && null (encode ([] :: [Integer]))
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs =
    case pack xs of
        [] -> []
        (x:_) -> (length x, head x) : encode (drop  (length x) xs)

