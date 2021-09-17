import Data.List
import System.Random

-- 22
testRange =
    range 4 9 == [4,5,6,7,8,9]
    && range 1 10 == [1..10]
    && range 30 40 == [30..40]
range :: Integral a => a -> a -> [a]
range start end = takeWhile (<= end) $ iterate (+ 1) start
