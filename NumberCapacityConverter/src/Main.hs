module Main where

import System.Environment
import Data.Char

main = do
    (inputCapacityStr:outputCapacityStr:input:_) <- getArgs
    let inputCapacity = read inputCapacityStr :: Int
        outputCapacity = read outputCapacityStr :: Int
    print (toDecimal inputCapacity input)

type Multiplier = Int
type Result = Int

toDecimal :: Int -> String -> Int
toDecimal inputCapacity = snd . foldr (toDecimalFold . digitToInt) (1, 0)
    where toDecimalFold :: Int -> (Multiplier, Result) -> (Multiplier, Result)
          toDecimalFold digit (multiplier, result) = (multiplier * inputCapacity, result + digit * multiplier)

-- toCapacity :: Int -> Int -> String
-- toCapacity _ 0 = "0"
-- toCapacity outputCapacity number =
