module Sorters.InsertSort (insertSort) where

import Data.List (insert)

insertSort :: Ord a => [a] -> [a]
insertSort = doInsertSort []

doInsertSort :: Ord a => [a] -> [a] -> [a]
doInsertSort sorted [] = sorted
doInsertSort sorted [lastUnsorted] = insert lastUnsorted sorted
doInsertSort sorted (firstUnsorted:unsorted) = doInsertSort (insert firstUnsorted sorted) unsorted
