module Sorters.SelectionSort (selectionSort) where

selectionSort :: Ord a => [a] -> [a]
selectionSort = doSelectionSort []

(!!=) :: [a] -> (Int, a) -> [a]
xs !!= (n, x) = take n xs ++ [x] ++ drop (n + 1) xs

doSelectionSort :: Ord a => [a] -> [a] -> [a]
doSelectionSort sorted [] = sorted
doSelectionSort sorted [lastUnsorted] = sorted ++ [lastUnsorted]
doSelectionSort sorted (firstUnsorted:unsorted)
    | minIndex == 0 = doSelectionSort (sorted ++ [firstUnsorted]) unsorted
    | otherwise     = doSelectionSort (sorted ++ [minElem]) (unsorted !!= (minIndex - 1, firstUnsorted))
    where (minElem, minIndex) = minimumAndIndex (firstUnsorted:unsorted)

minimumAndIndex :: Ord a => [a] -> (a, Int)
minimumAndIndex [] = error "Sorters.SelectionSort.minimumAndIndex: empty list"
minimumAndIndex (x:xs) = doMinimumAndIndex x xs

doMinimumAndIndex :: Ord a => a -> [a] -> (a, Int)
doMinimumAndIndex cm [] = (cm, 0)
doMinimumAndIndex cm (x:xs)
    | cm <= x   = (cm, 0)
    | otherwise = let (nm, ni) = doMinimumAndIndex x xs in (nm, ni + 1)
