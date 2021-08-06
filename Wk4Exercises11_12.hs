module Wk4Exercises11_12 where
import Wk2Exercises6_10

data EncodedElement a = Single a | Multiple Int a
    deriving (Eq, Show)

testEncodeModified :: Bool
testEncodeModified =
    encodeModified "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
    && encodeModified [1, 2, 3, 4] == [Single 1, Single 2, Single 3, Single 4]
    && null (encodeModified ([] :: [Integer]))
encodeModified :: (Eq a) => [a] -> [EncodedElement a]
encodeModified xs = 
    map singleOut (Wk2Exercises6_10.encode xs)
    where singleOut (1, y) = Single y
          singleOut (x, y) = Multiple x y

testDecodeModified :: Bool
testDecodeModified =
    decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'] == "aaaabccaadeeee"
    && decodeModified [Single 1, Single 2, Single 3, Single 4] == [1, 2, 3, 4]
    && null (decodeModified ([] :: [EncodedElement Integer]))
decodeModified :: (Eq a) => [EncodedElement a] -> [a]
decodeModified [] = []
decodeModified (Single x:xs) = x : decodeModified xs
decodeModified (Multiple i x:xs) = replicate i x ++ decodeModified xs
