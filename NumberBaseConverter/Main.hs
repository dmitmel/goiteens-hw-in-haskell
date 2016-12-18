module Main where

import System.Environment
import Data.Char (digitToInt, intToDigit)
import Data.List (foldl')

main :: IO ()
main = do
    (inputBaseStr:outputBaseStr:input:_) <- getArgs
    let inputBase  = read inputBaseStr  :: Int
        outputBase = read outputBaseStr :: Int
    print (toDecimal inputBase input)

digitToIntBase :: Int -> Char -> Int
digitToIntBase base c
    | digit >= base = error $ "digitToIntBase: digit (" ++ show digit ++ ") is greater than base (" ++ show base ++ ")"
    | otherwise     = digit
    where digit = digitToInt c

toDecimal :: Int -> String -> Int
toDecimal base = foldl' (\acc x -> acc * base + digitToIntBase base x) 0

toBinary :: Int -> String
toBinary num = reverse $ fst $ foldr (\m (bin, num_) -> if m <= num_ then ('1':bin, num_ - m) else ('0':bin, num_)) ("", num) $ takeWhile (<= num) $ iterate (*2) 1

intToDigitSafe :: Int -> Int -> Char
intToDigitSafe base i
    | i > 15    = error $ "intToDigitSafe: int (" ++ show i ++ ") is greater than 15"
    | i >= base = intToDigit $ base - 1
    | otherwise = intToDigit i

toBase :: Int -> Int -> String
toBase _          0   = "0"
toBase 2          num = reverse $ fst $ foldr (\m (bin, num_) -> if m <= num_ then ('1':bin, num_ - m) else ('0':bin, num_)) ("", num) $ takeWhile (<= num) $ iterate (*2) 1
toBase 10         num = show num
-- toBase outputBase num = reverse $ fst $ foldr (\m (bin, num_) -> if m <= num_ then (intToDigitSafe outputBase m : bin, num_ - m) else ('0':bin, num_)) ("", num) $ takeWhile (<= num) $ iterate (*outputBase) 1
