import Control.Monad.Trans.Writer (Writer, writer)
import Control.Monad (foldM)

type Item = Int
type ID = Int
data Monkey = Monkey {id' :: ID, items :: [Item], op :: Item -> Item, test :: Item -> Bool, trueDest :: ID, falseDest :: ID}

-- parseFile :: [String] -> [Monkey]

-- parseMonkey :: [String] -> Monkey

executeRounds :: Int -> [Monkey] -> Writer [(ID, [Item])] [Monkey]
executeRounds 0 monkeys = writer (monkeys, map summarizeMonkey monkeys)
executeRounds n monkeys = executeRound monkeys >>= executeRounds (n - 1)

-- executeRound :: [Monkey] -> Writer [(ID, [Item])] [Monkey]

summarizeMonkey :: Monkey -> (ID, [Item])
summarizeMonkey Monkey {id'=_id, items=_items} = (_id, _items)
