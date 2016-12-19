module Main where

import           Control.Exception     (finally)
import           Control.Monad         (forM_)
-- import           Criterion.Main
import           Sorters.BubbleSort
import           Sorters.InsertSort
import           Sorters.MergeSort
import           Sorters.QuickSort
import           Sorters.SelectionSort
import           System.Console.ANSI
import           System.Environment    (getArgs)
import           System.Random
import           Text.Printf           (printf)

randomNumbers :: (Num a, Random a) => Int -> [a]
randomNumbers count = take count $ randomRs (0, fromIntegral count) (mkStdGen 0)

testSortAlgorithm :: Show a => ([a] -> [a]) -> Int -> [a] -> IO ()
testSortAlgorithm algorithm dataLength data_ =
    let sorted           = algorithm data_
        sortedWithSteps  = zip [1..dataLength] sorted
        oneStepIncrement = (100 :: Double) / (fromIntegral dataLength :: Double)
    in do
        hideCursor
        putChar '\n'
        forM_ sortedWithSteps (\(step, sortedElem) -> do
            cursorUpLine 1
            let completePercent = oneStepIncrement * fromIntegral step
            printf "[%s>%s] %.2f%%, step %i/%i, %s\n"
                (replicate (ceiling completePercent - 1) '=')
                (replicate (100 - ceiling completePercent) ' ')
                completePercent step dataLength
                (show sortedElem)
            ) `finally` do
                cursorUpLine 1
                clearLine
                showCursor

-- createBenchmark :: String -> [a] -> ([a] -> [a]) -> Benchmark
-- createBenchmark name values algorithm = bgroup name $ sortedBenchmarks ++ unsortedBenchmarks
--     where
--         sortedBenchmarks = map values (\value -> bench ("sorted " ++ show value) $ nfIO (testSortAlgorithm algorithm (length values) values))
--         unsortedBenchmarks = map values (\value -> bench ("unsorted " ++ show value) $ nfIO (testSortAlgorithm algorithm (length values) values))

main :: IO ()
-- main = defaultMain
--     [ bgroup "mergeSort"
--       [ bench "sorted 10" $ nfIO (testSortAlgorithm mergeSort 10 ([1..10] :: [Int]))
--       , bench "sorted 100" $ nfIO (testSortAlgorithm mergeSort 100 ([1..100] :: [Int]))
--       , bench "sorted 1000" $ nfIO (testSortAlgorithm mergeSort 1000 ([1..1000] :: [Int]))
--       , bench "sorted 10000" $ nfIO (testSortAlgorithm mergeSort 10000 ([1..10000] :: [Int]))
--       , bench "sorted 100000" $ nfIO (testSortAlgorithm mergeSort 100000 ([1..100000] :: [Int]))
--       , bench "unsorted 10" $ nfIO (testSortAlgorithm mergeSort 10 (randomNumbers 10 :: [Int]))
--       , bench "unsorted 100" $ nfIO (testSortAlgorithm mergeSort 100 (randomNumbers 100 :: [Int]))
--       , bench "unsorted 1000" $ nfIO (testSortAlgorithm mergeSort 1000 (randomNumbers 1000 :: [Int]))
--       , bench "unsorted 10000" $ nfIO (testSortAlgorithm mergeSort 10000 (randomNumbers 10000 :: [Int]))
--       , bench "unsorted 100000" $ nfIO (testSortAlgorithm mergeSort 100000 (randomNumbers 100000 :: [Int]))
--       ]
--     ]
main = do
    [algorithmName, countStr] <- getArgs
    let algorithm = getAlgorithm algorithmName
        count     = read countStr :: Int
    testSortAlgorithm algorithm count (randomNumbers count :: [Int])

getAlgorithm :: Ord a => String -> ([a] -> [a])
getAlgorithm "bubble"    = bubbleSort
getAlgorithm "insert"    = insertSort
getAlgorithm "merge"     = mergeSort
getAlgorithm "quick"     = quickSort
getAlgorithm "selection" = selectionSort
getAlgorithm algorithm   = error $ "Unknown algorithm: " ++ algorithm
