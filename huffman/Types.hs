data Bit = L | R deriving (Eq, Show)

type HCode = [Bit]

type Table = [(Char, HCode)]

data Tree = Leaf Char Int
            | Node Int Tree Tree
