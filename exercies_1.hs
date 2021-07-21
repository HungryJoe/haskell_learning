myLast xs = elementAt xs (myLength xs)
testMyLast = (myLast [1,2,3,4] == 4) && (myLast ['a'..'z'] == 'z')

myButLast xs = elementAt xs (myLength xs - 1)
testMyButLast = (myButLast [1,2,3,4] == 3) && (myButLast ['a'..'z'] == 'y')

elementAt xs idx = head (elementAtRecursive xs (myLength xs) idx)
elementAtRecursive xs total_len idx = if total_len - myLength (tail xs) == idx then [head xs] else elementAtRecursive (tail xs) total_len idx
testElementAt = (elementAt [1,2,3] 2 == 2) && (elementAt "haskell" 5 == 'e')

myLength xs = sum [1 | _ <- xs]
testMyLength = (myLength [123, 456, 789] == 3) && (myLength "Hello, world!" == 13)

myReverse xs = if myLength xs == 1 then xs else myLast xs : myReverse (myInit xs)
testMyReverse = (myReverse "A man, a plan, a canal, panama!" == "!amanap ,lanac a ,nalp a ,nam A") && (myReverse [1,2,3,4] == [4,3,2,1])

myInit xs = if myLength xs == 1 then [] else head xs : myInit (tail xs)
testMyInit = (myInit [1, 2, 3] == [1, 2]) && (myInit "hello" == "hell")
