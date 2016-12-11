module Cryptography
( Alphabet(..), english, russian
, alphabetOrd, alphabetIgnoreCaseOrd, alphabetChr, alphabetLength
, alphabetElem, notAlphabetElem
, isLowercase, isUppercase
, shiftChar, shift
) where

import Data.List
import Data.Maybe

data Alphabet = Alphabet { lowercase :: String
                         , uppercase :: String } deriving (Show)

english :: Alphabet
english = Alphabet ['a'..'z'] ['A'..'Z']

russian :: Alphabet
russian = Alphabet ['\1072'..'\1103'] ['\1040'..'\1071']

alphabetOrd :: Alphabet -> Char -> Int
alphabetOrd alphabet c
    | alphabet `isLowercase` c =                fromMaybe (error $ "Cryptography.alphabetOrd: " ++ show c ++ " is not in alphabet") $ elemIndex c lower
    | alphabet `isUppercase` c = length lower + fromMaybe (error $ "Cryptography.alphabetOrd: " ++ show c ++ " is not in alphabet") (elemIndex c upper)
    | otherwise                = error $ "Cryptography.alphabetOrd: " ++ show c ++ " is not in alphabet"
    where lower = lowercase alphabet
          upper = uppercase alphabet

alphabetIgnoreCaseOrd :: Alphabet -> Char -> Int
alphabetIgnoreCaseOrd alphabet c
    | alphabet `isLowercase` c = fromMaybe (error $ "Cryptography.alphabetOrd: " ++ show c ++ " is not in alphabet") $ elemIndex c lower
    | alphabet `isUppercase` c = fromMaybe (error $ "Cryptography.alphabetOrd: " ++ show c ++ " is not in alphabet") $ elemIndex c upper
    | otherwise                = error $ "Cryptography.alphabetOrd: " ++ show c ++ " is not in alphabet"
    where lower = lowercase alphabet
          upper = uppercase alphabet

alphabetChr :: Alphabet -> Int -> Char
alphabetChr (Alphabet lower upper) i
    | i < 0                                    = error $ "Cryptography.alphabetChr: index (" ++ show i ++ ") is too small"
    | i >= 0        && i < lowerLen            = lower !! i
    | i >= lowerLen && i < lowerLen + upperLen = upper !! (i - lowerLen)
    | otherwise                                = error $ "Cryptography.alphabetChr: index (" ++ show i ++ ") is too large"
    where lowerLen = length lower
          upperLen = length upper

alphabetLength :: Alphabet -> Int
alphabetLength (Alphabet lower upper)
    | lowerLen /= upperLen = error $ "Cryptography.alphabetLength: count of lower letters (" ++ show lowerLen ++ ") differs to count of upper letters (" ++ show upperLen ++ ") in alphabet"
    | otherwise = lowerLen -- or upperLen
    where lowerLen = length lower
          upperLen = length upper

alphabetElem :: Char -> Alphabet -> Bool
alphabetElem c (Alphabet lower upper) = c `elem` lower
                                     || c `elem` upper

notAlphabetElem :: Char -> Alphabet -> Bool
notAlphabetElem c a = not $ alphabetElem c a

isLowercase :: Alphabet -> Char -> Bool
isLowercase (Alphabet lower _) c = c `elem` lower

isUppercase :: Alphabet -> Char -> Bool
isUppercase (Alphabet _ upper) c = c `elem` upper

shiftChar :: Int -> Alphabet -> Char -> Char
shiftChar 0 _ c = c
shiftChar s alphabet c
    | alphabet `isLowercase` c = let lower = lowercase alphabet in lower !! (((alphabet `alphabetIgnoreCaseOrd` c) + s) `mod` length lower)
    | alphabet `isUppercase` c = let upper = uppercase alphabet in upper !! (((alphabet `alphabetIgnoreCaseOrd` c) + s) `mod` length upper)
    | otherwise                = error $ "Cryptography.shiftChar: " ++ show c ++ " is not in alphabet"

shift :: Int -> Alphabet -> String -> String
shift 0 _ str        = str
shift _ _ ""         = ""
shift s alphabet str = map (\c -> if c `notAlphabetElem` alphabet then c else shiftChar s alphabet c) str
