module Coding ( codeMessage
              , decodeMessage)


import Types (Tree(Leaf, Node), Bit(L, R), HCode, Table)


codeMessage tbl = concat . map (lookupTable tbl)

decodeMessage tr
  = decodeByt tr
    where
      decodeByt (Node n t1 t2) (L:rest) = decodeByt t1 rest
      decodeByt (Node n t1 t2) (R:rest) = decodeByt t2 rest
      decodeByt (Leaf c n) rest         = c : decodeByt tr rest
      decodeByt t []                    = []


lookupTable :: Table -> Char -> HCode
lookupTable [] c = error "lookupTable"
lookupTable ((ch, n):tb) c
  | ch == c     = n
  | otherwise   = lookupTable tb c

