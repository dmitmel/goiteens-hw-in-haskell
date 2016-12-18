module Sorters.MergeSort (mergeSort) where

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = join (mergeSort left) (mergeSort right)
    where (left, right) = split2 xs

split2 :: [a] -> ([a], [a])
split2 xs = (take (len `div` 2) xs, drop (len `div` 2) xs)
    where len = length xs

join :: Ord a => [a] -> [a] -> [a]
join [] ys = ys
join xs [] = xs
join (x:xs) (y:ys)
    | x <= y    = x:join xs (y:ys)
    | otherwise = y:join (x:xs) ys
