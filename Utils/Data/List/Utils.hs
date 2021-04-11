module Data.List.Utils where

import           Debug.Trace

toBlocks :: Int -> [a] -> [[a]]
toBlocks _    [] = []
toBlocks size xs = let block = take size xs
                       ys    = drop size xs
                   in block : toBlocks size ys

(!!=) :: [a] -> (Int, a) -> [a]
xs !!= (n, x) = take n xs ++ [x] ++ drop (n + 1) xs

rotateRight :: Int -> [a] -> [a]
rotateRight shift_ xs = map (\i -> xs !! ((i + shift_) `mod` len)) [0..len - 1]
    where len = length xs

rotateLeft :: Int -> [a] -> [a]
rotateLeft shift_ xs = map (\i -> xs !! ((i - shift_) `mod` len)) [0..len - 1]
    where len = length xs


deleteAt :: Int -> [a] -> [a]
deleteAt _ []     = error "Data.List.Utils.deleteAt: index out of range"
deleteAt 0 (x:xs) = xs
deleteAt i xs
    | i < 0 || i >= length xs = error "Data.List.Utils.deleteAt: index out of range"
    | otherwise = take i xs ++ drop (i + 1) xs

leftHalf :: [a] -> [a]
leftHalf [] = error "Data.List.Utils.leftHalf: empty list"
leftHalf xs = take (length xs `div` 2) xs

middle :: [a] -> a
middle [] = error "Data.List.Utils.middle: empty list"
middle xs = xs !! (length xs `div` 2)

rightHalf :: [a] -> [a]
rightHalf [] = error "Data.List.Utils.rightHalf: empty list"
rightHalf xs = drop ((length xs `div` 2) + 1) xs

binarySearch :: (Show a, Ord a) => [a] -> a -> Maybe Int
binarySearch []  x = Nothing
binarySearch [y] x
    | x == y    = Just 0
    | otherwise = Nothing
binarySearch xs  x
    | x == y = Just middleI
    | x <  y = binarySearch left x
    | x >  y = case binarySearch right x of
        Just rightI -> Just $ 1 + middleI + rightI
        Nothing     -> Nothing
    where
        left    = leftHalf xs
        middleI = length xs `div` 2
        y       = middle xs
        right   = rightHalf xs
