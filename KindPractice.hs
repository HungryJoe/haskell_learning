data Job a = Bob a deriving Show
instance Functor Job where
    fmap f (Bob a) = Bob (f a)
-- Ex. fmap (:[]) (Bob 1)

-- From the book:
class Tofu t where  
    tofu :: j a -> t a j
data Frank a b = Frank{frankField :: b a} deriving Show
instance Tofu Frank where
    tofu x = Frank x
-- Ex. tofu "abc" :: Frank Char []

-- Also from the book:
data Barry t k p = Barry { yabba :: p, dabba :: t k } deriving Show
instance Functor (Barry a b) where
    fmap f Barry{yabba=yabba, dabba=dabba} = Barry{yabba = f yabba, dabba = dabba}
-- Ex. fmap read Barry{yabba="1.2", dabba=Just 12} :: Barry Maybe Int Float
