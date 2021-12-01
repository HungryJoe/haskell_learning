-- 19
testRotLeft :: Bool
testRotLeft =
    rotLeft ['a'..'h'] 3 == "defghabc"
    && rotLeft ['a'..'h'] (-2) == "ghabcdef"
    && null (rotLeft [] 10)
rotLeft :: (Integral b) => [a] -> b -> [a]
rotLeft xs rot_by =
    let append out x = out ++ [x]
    in let cutPoint
            | rot_by >= 0 = fromIntegral rot_by
            | otherwise = fromIntegral (length xs) + fromIntegral rot_by
    in
    foldl append [] (drop cutPoint xs) ++ foldr (:) [] (take cutPoint xs)

-- testRotLeftBy1 = 
--     rotLeftBy1 ['a'..'h'] == "bcdefgha"
--     && rotLeftBy1 [1..10] == [2,3,4,5,6,7,8,9,10,1]
-- rotLeftBy1 :: [a] -> [a]
-- rotLeftBy1 xs =
--     let blah (i, x) out 
--             | i == 1 = out ++ [x] 
--             | otherwise = x : out in
--     foldr blah [] $ zip [1..] xs

-- 20
testRemoveAt =
    removeAt 2 "abcd" == ('b',"acd")
    && removeAt 5 [1..10] == (5, [1, 2, 3, 4, 6, 7, 8, 9, 10])
removeAt :: (Integral b) => b -> [a] -> (a, [a])
removeAt idx =
    let removeCheck (i, x) y
            | i == idx = (x, snd y)
            | otherwise = (fst y, x : snd y)
    in foldr removeCheck (undefined, []) . zip [1..]

-- 21
testInsertAt =
    insertAt 'a' "efgh" 1 == "aefgh"
    && insertAt 5 [1..10] 10 == [1,2,3,4,5,6,7,8,9,5,10]
    && insertAt 'X' "abcd" 2 == "aXbcd"
    && insertAt 1 [] 1 == [1]
insertAt :: (Integral b) => a -> [a] -> b -> [a]
insertAt new [] 1 = [new]
insertAt new xs pos =
    let tryInsert (i, x) ys
            | i == pos = new : x : ys
            | otherwise = x : ys
    in foldr tryInsert [] $ zip [1..] xs
