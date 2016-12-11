{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Cryptography.RSA
( SecretKey(..), OpenKey(..)
, generateRandomKeys, generateKeys
, encryptNumber, decryptNumber
, encrypt, decrypt
) where

import Data.List
import Data.Maybe
import Data.Char
import Cryptography.RSA.Random
import Math.NumberTheory.Primes
import System.Random (RandomGen)

data SecretKey = SecretKey { secretD :: Integer, secretN :: Integer } deriving (Show)

data OpenKey = OpenKey { openE :: Integer, openN :: Integer } deriving (Show)

isCoprime :: Integral a => a -> a -> Bool
isCoprime a b = gcd a b == 1

generateRandomKeys :: RandomGen g => Int -> g -> ((SecretKey, OpenKey), g)
generateRandomKeys bitsCount p1Gen = let (p1, p2Gen)     = randomPrime bitsCount p1Gen
                                         (p2, resultGen) = randomPrime bitsCount p2Gen
                                     in (generateKeys p1 p2, resultGen)

generateKeys :: Integer -> Integer -> (SecretKey, OpenKey)
generateKeys p1 p2 = (SecretKey d n, OpenKey e n)
    where
        n   = p1 * p2
        phi = (p1 - 1) * (p2 - 1)
        e   = head $ filter (isCoprime phi) $ takeWhile (< phi) primes
        d   = fromJust $ find (\d_ -> (e * d_) `mod` phi == 1) [1..]

encryptNumber :: OpenKey -> Integer -> Integer
encryptNumber (OpenKey e n) num = (num ^ e) `mod` n

decryptNumber :: SecretKey -> Integer -> Integer
decryptNumber (SecretKey d n) c = (c ^ d) `mod` n

encrypt :: Show a => OpenKey -> a -> [Integer]
encrypt key = map (encryptNumber key . toInteger . ord) . show

decrypt :: Read a => SecretKey -> [Integer] -> a
decrypt key = read . map (chr . fromInteger . decryptNumber key)
