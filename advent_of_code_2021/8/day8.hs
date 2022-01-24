-- Daniel Lyons' solution
module Main where

import Data.List (sort, permutations)
import qualified Data.Map as Map
import Data.Maybe
import Data.Map ((!))

{-

For this day, we have a 7-segment display, that looks like this:

  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg


Our problem is that we are given inputs that are sequences of segments but we don't know which segment really maps to which number. So we need to make some guesses first by using the length of the segment. It turns out some of the possible values can be discerned simply by the length of the segment sequence.

┌──────────────┬─────────────────┐
│ Segments Lit │ Possible Values │
├──────────────┼─────────────────┤
│            2 │               1 │
│            3 │               7 │
│            5 │         2, 3, 5 │
│            4 │               4 │
│            6 │         0, 6, 9 │
│            7 │               8 │
└──────────────┴─────────────────┘

The first part of the challenge asks us to count up the number of 1s, 7s, 4s, and 8s, but it seems clear that the second part of the challenge will require us to fully decode the integers. So a general solution is probably not a bad idea.

What's really going on here is that the output is a permutation of a-g, and we have to discover what that permutation is. So let's think about what segments could be involved in forming which digits:

┌─────────┬───────────────────────────┐
│ Segment │      Possible Values      │
├─────────┼───────────────────────────┤
│ a       │    0, 2, 3, 5, 6, 7, 8, 9 │
│ b       │          0, 4, 5, 6, 8, 9 │
│ c       │    0, 1, 2, 3, 4, 7, 8, 9 │
│ d       │       2, 3, 4, 5, 6, 8, 9 │
│ e       │                0, 2, 6, 8 │
│ f       │ 0, 1, 3, 4, 5, 6, 7, 8, 9 │
│ g       │       0, 2, 3, 5, 6, 8, 9 │
└─────────┴───────────────────────────┘

The permutation we're looking for is going to establish a mapping from one segment to another. The mapping has to be a bijection, because it has to be one-to-one and onto, because we have to have all seven segments going in and all seven segments going out.

We can enumerate all the possible permutations. Each of these is like a hypothesis. Then we can use a fact from the input to reject some of the hypotheses. For instance, if we get an input like "ab", we know that a satisfactory hypothesis will have one of these mappings:

    1)  c -> a  &&  f -> b
    2)  c -> b  &&  f -> a

All the hypotheses that do not have one of these structures can be discarded.

Now we know that 0, 6, and 9 have six segments lit, and 0 and 9 require both c and f. But 6 requires only c. So if in our list of unique digits we have three inputs of length 6 that each have "b", we know that the correct mapping takes c -> b and f -> a. So we can discard the other.

So this suggests that a hypothesis supports some kind of inquiry, so you can ask it whether it maps c -> b or c -> a, and it will return true or false.

Another view of the hypothesis is as a function that takes some sequence like "bca" and maps it to an integer like 7. 

-}

digitMap = Map.fromList 
    [("abcefg", 0),
     ("cf", 1),
     ("acdeg", 2),
     ("acdfg", 3),
     ("bcdf", 4),
     ("abdfg", 5),
     ("abdefg", 6),
     ("acf", 7),
     ("abcdefg", 8),
     ("abcdfg", 9)]

type Segment = Char
type Sequence = String
type Hypothesis = Map.Map Segment Segment

-- `maps` allows us to test whether a hypothesis maps a certain segment to 
-- another segment. We will use this to filter out candidate permutations.
maps :: Hypothesis -> (Segment, Segment) -> Bool
hypothesis `maps` (source, dest) = hypothesis ! source == dest

-- `evaluate` allows us to convert a sequence to a digit according to a hypothesis
-- we will need this in the end to process the input when we have reduced to a
-- single hypothesis.
evaluate :: Hypothesis -> Sequence -> Integer
evaluate hypothesis sequence = fromJust $ mayEvaluate hypothesis sequence

mayEvaluate :: Hypothesis -> Sequence -> Maybe Integer
mayEvaluate hypothesis sequence = segmentsToDigit $ sort $ fmap decode sequence
    where
        segmentsToDigit = flip Map.lookup digitMap
        decode = (hypothesis !)

-- the identity hypothesis, for testing purposes
ident :: Hypothesis
ident = Map.fromList [(c, c) | c <- "abcdefg"]

{-

Now that we are able to express some of the ideas from the domain in our own terms, we can start to build up the knowledge that we have constructed by hand above. Let's start by building the possible values map.

-}

segmentToPossibleValues :: Map.Map Segment [Integer]
segmentToPossibleValues = Map.fromList 
    [ (char, 
       sort [ digit | (sequence, digit) <- Map.toList digitMap, char `elem` sequence]) 
       | char <- "abcdefg" ]

{- 

OK now it seems we need to know the possible digits based on the sequence length

-}

possibleDigitBySequenceLength :: Map.Map Int [Integer]
possibleDigitBySequenceLength = Map.fromListWith (++) [(length sequence, [digit]) | (sequence, digit) <- Map.toList digitMap]


{-

The remaining unaddressed issue here is that it seems we are able to make a "guess" as to what the result is based on the length. So it may not be necessary to converge to a single hypothesis; a family of similar hypotheses that only differ in terms of how they represent digits that don't appear may still converge for possible decodings. For instance, we can always report that a two-character sequence is 1.

It may be sufficient here to create a function of type Maybe that tries to decode using a list of hypotheses, and reports Just a result if there is just one possible result, and Nothing in cases where there are too many possibilities.

-}

-- ???

{-

OK, so now let's think about how to solve this problem actually.

The algorithm I have in mind is this:

 - Generate all the permutations of 7 segments that are possible. This is the hypothesis list. This is just 5,040 entries in size, so it isn't a huge fear, although we do need to create it for each display.
 - Filter out the hypotheses that are possible based on the 10 unique digit patterns we have encountered.
 - Ideally, we get one legitimate hypothesis for each display. If so, we can ask it to decode the four digits we get on the other side of the bar.
 - In non-ideal circumstances, we can feed the remaining hypotheses to the partial decoder and see if it is able to generate the right digits for the inputs on the other side of the bar.

Before we can make much more progress, we need to handle parsing the input.

-}

-- create a hypothesis from a certain permutation of "abcdefg"
createHypothesis :: String -> Hypothesis
createHypothesis permutation = Map.fromList $ zip "abcdefg" permutation

-- these are all the possible hypotheses
allHypotheses :: [Hypothesis]
allHypotheses = map createHypothesis (permutations "abcdefg")

{- 

So, we have a set of ten unique inputs. These are probably supposed to represent the ten digits, but the instructions did not guarantee that. Nevertheless, we can imagine proceeding like so:

 - Consider each of these inputs in turn.
 - Consider the length of the input. Because we know the lengths are only possible for certain digits, we can infer that only those digits are possible readings of this input.
 - We can consider every permutation of these segments a possible mapping to any of these digits.

-}

-- sequence `supports` hypothesis if sequence decodes to a digit in hypothesis
supports :: Sequence -> Hypothesis -> Bool
sequence `supports` hypothesis = isJust $ mayEvaluate hypothesis sequence

sampleInput = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"


-- A display has some unique sequences and then the four-digit display
type Display = ([Sequence], [Sequence])
parse :: String -> [Display]
parse = map parseLine . lines

-- parse a single line of the input into a display
parseLine :: String -> Display
parseLine line = ([d1,d2,d3,d4,d5,d6,d7,d8,d9,d10], [i1,i2,i3,i4])
    where
        [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,"|",i1,i2,i3,i4] = words line

-- the idea here is to filter out impossible hypotheses using the inputs we were given
-- I feel like this should be expressable as a fold but I just don't see how to code it
removeImpossible :: [Sequence] -> [Hypothesis] -> [Hypothesis]
removeImpossible [] hypotheses = hypotheses
removeImpossible (sample:samples) hypotheses = removeImpossible samples (filter (supports sample) hypotheses)

-- Slightly gross, but I want to ensure that the application explodes if my algorithm fails
search :: [Sequence] -> Hypothesis
search sequence = result 
    where [result] = removeImpossible sequence allHypotheses

-- decode converts a display to its numeric values, so you get back a list of four things like [3,9,4,2]
decode :: Display -> [Integer]
decode (inputs, outputs) = map (evaluate (search inputs)) outputs

-- since I did everything up-front, I can just count the digits after decoding everything to do part 1
part1 :: IO Int
part1 = do
    displays <- parse <$> readFile "day8.txt"
    let decoded = concatMap decode displays
    return $ length (filter (`elem` [1,4,7,8]) decoded)

-- for part two, we need to get all the way to the actual numeric value, so we need a helper function

-- decodeToInteger converts a display to its actual numeric value, so from the above you would get 3942
-- there are obviously better ways to do this, but since I always have four digits I can be lazy
decodeToInteger :: Display -> Integer
decodeToInteger display = d1 * 1000 + d2 * 100 + d3 * 10 + d4
    where [d1,d2,d3,d4] = decode display

-- for part 2, we have to decode to integers and sum them, but that's not much worse
part2 :: IO Integer
part2 = do
    displays <- parse <$> readFile "day8.txt"
    let decoded = map decodeToInteger displays
    return $ sum decoded

main = do
    part1out <- part1
    putStrLn $ "Part 1: " ++ (show part1out)
    part2out <- part2
    putStrLn $ "Part 2: " ++ (show part2out)
