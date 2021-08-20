-- 16
testDropEvery :: Bool
testDropEvery =
    "abcdefghik" `dropEvery` 3 == "abdeghk"
    && null ([1..5] `dropEvery` 1)
    && [1..50] `dropEvery` 2 == [1,3..50]
    && ['a'..'z'] `dropEvery` 4 == "abcefgijkmnoqrsuvwyz"
    && null ([] `dropEvery` 1000)
dropEvery :: [a] -> Int -> [a]
[] `dropEvery` n = []
xs `dropEvery` n =
    let [] `dropOne` i = []
        (y:ys) `dropOne` 1 = ys `dropOne` n
        (y:ys) `dropOne` i = y : (ys `dropOne` (i - 1))
    in xs `dropOne` n

-- 17
testSplit :: Bool
testSplit =
    "abcdefghik" `split` 3 == ("abc", "defghik")
    && [1..10] `split` 5 == ([1..5], [6..10])
    && ([] :: [Char]) `split` 2 == ([], [])
    && [1..3] `split` 5 == ([1..3], [])
split :: [a] -> Int -> ([a], [a])
[] `split` n = ([], [])
(x:xs) `split` 1 = ([x], xs)
(x:xs) `split` n =
    let (splitFst, splitSnd) = xs `split` (n - 1)
    in (x : splitFst, splitSnd)

-- 18
testSlice :: Bool
testSlice =
    slice ['a'..'k'] 3 7 == ['c'..'g']
    && slice [1..20] 4 10 == [4..10]
    && slice [1..10] 3 13 == [3..10]
    && null (slice [1..5] 6 30)
    && null (slice [] 1 4)
slice :: [a] -> Int -> Int -> [a]
slice [] start end = []
slice xs start end =
    let sliceStart [] beg = []
        sliceStart ys 1 = ys
        sliceStart (y:ys) beg = sliceStart ys (beg - 1)
        sliceEnd [] term = []
        sliceEnd (y:ys) 0 = [y]
        sliceEnd (y:ys) term = y : sliceEnd ys (term - 1)
    in sliceEnd (sliceStart xs start) (end - start)

