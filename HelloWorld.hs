import Data.Char
import Control.Monad

-- main = putStrLn "Hello World!"
--
-- main = do
--     putStrLn "What's your name???"
--     name <- getLine
--     let upperName = map toUpper name
--     print $ "Hi, " ++ upperName
--
main = forM [1..5] print
