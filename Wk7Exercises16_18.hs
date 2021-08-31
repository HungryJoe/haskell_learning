-- 16
testDropEvery :: Bool
testDropEvery =
    "abcdefghik" `dropEvery` 3 == "abdeghk"
    && null ([1..5] `dropEvery` 1)
    && [1..50] `dropEvery` 2 == [1,3..50]
    && ['a'..'z'] `dropEvery` 4 == "abcefgijkmnoqrsuvwyz"
    && null ([] `dropEvery` 1000)
dropEvery :: Integral b => [a] -> b -> [a]
dropEvery xs freq = [x | (x, i) <- zip xs [1..], i `mod` freq /= 0]

-- 17
testSplit :: Bool
testSplit =
    "abcdefghik" `split` 3 == ("abc", "defghik")
    && [1..10] `split` 5 == ([1..5], [6..10])
    && ([] :: [Char]) `split` 2 == ([], [])
    && [1..3] `split` 5 == ([1..3], [])
split :: Integral b => [a] -> b -> ([a], [a])
split xs len =
    let segment cmp = [x | (x, i) <- zip xs [1..], i `cmp` len]
    in (segment (<=), segment (>))

--18
testSlice :: Bool
testSlice =
    slice ['a'..'k'] 3 7 == ['c'..'g']
    && slice [1..20] 4 10 == [4..10]
    && slice [1..10] 3 13 == [3..10]
    && null (slice [1..5] 6 30)
    && null (slice [] 1 4)
slice :: Integral b => [a] -> b -> b -> [a]
slice xs start end = [x | (x, i) <- zip xs [1..end], i >= start]
