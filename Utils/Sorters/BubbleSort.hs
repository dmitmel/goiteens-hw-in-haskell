module Sorters.BubbleSort (bubbleSort) where

bubbleSort :: Ord a => [a] -> [a]
bubbleSort = doBubbleSort []

doBubbleSort :: Ord a => [a] -> [a] -> [a]
doBubbleSort sorted []              = sorted
doBubbleSort sorted [firstUnsorted] = firstUnsorted : sorted
doBubbleSort sorted unsorted        = doBubbleSort (last swapped : sorted) (init swapped)
    where swapped = swapUnsorted unsorted

swapUnsorted :: Ord a => [a] -> [a]
swapUnsorted []  = []
swapUnsorted [x] = [x]
swapUnsorted (first:second:xs)
    | first > second = second:swapUnsorted (first:xs)
    | otherwise      = first:swapUnsorted (second:xs)
