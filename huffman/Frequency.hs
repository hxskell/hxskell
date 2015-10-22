module Frequency (frequency) where

mergeSort :: ([a] -> [a] -> [a]) -> [a] -> [a]
mergeSort merge xs
  | length xs < 2     = xs
  | otherwise
    = merge (mergeSort merge first)
            (mergeSort merge second)
      where
        first   = take half xs
        second  = drop half xs
        half    = div (length xs) 2


alphaMerge :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
alphaMerge xs [] = xs
alphaMerge [] ys = ys
alphaMerge xa@((p, n):xs) ya@((q, m):ys)
  | (p == q)  = (p, n+m):alphaMerge xs ys
  | (p < q)   = (p, n):alphaMerge xs ya
  | otherwise = (q, m):alphaMerge xa ys


freqMerge :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
freqMerge xs [] = xs
freqMerge [] ys = ys
freqMerge xa@((p, n):xs) ya@((q, m):ys)
  | (n < m || (n==m && p<q))
      = (p, n):freqMerge xs ys
  | otherwise
      = (q, m):freqMerge xa ys


frequency :: [Char] -> [(Char, Int)]
frequency =
  mergeSort freqMerge . mergeSort alphaMerge . map start
    where start ch = (ch, 1)
