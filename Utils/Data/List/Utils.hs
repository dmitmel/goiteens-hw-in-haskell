module Data.List.Utils where

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
