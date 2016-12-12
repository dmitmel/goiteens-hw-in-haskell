{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Cryptography.RSA
( SecretKey(..), OpenKey(..)
, generateRandomKeys, generateKeys
, encryptNumber, decryptNumber
, encrypt, decrypt
) where

import Data.Char
import Cryptography.RSA.Random
import Math.NumberTheory.Primes (primes)
import Math.NumberTheory.Moduli (powerMod)
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
        d   = modularInverse e phi

modularInverse :: Integer -> Integer -> Integer
modularInverse e phi = x `mod` phi
    where (_, x, _) = extendedEuclidean e phi

extendedEuclidean :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclidean a b
    | d < 0     = (-d, -x, -y)
    | otherwise = (d, x, y)
    where (d, x, y) = egcd a b

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = let (g, y, x) = egcd (b `mod` a) a
           in (g, x - ((b `div` a) * y), y)

encryptNumber :: OpenKey -> Integer -> Integer
encryptNumber (OpenKey e n) num = powerMod num e n

decryptNumber :: SecretKey -> Integer -> Integer
decryptNumber (SecretKey d n) c = powerMod c d n

encrypt :: Show a => OpenKey -> a -> [Integer]
encrypt key = map (encryptNumber key . toInteger . ord) . show

decrypt :: Read a => SecretKey -> [Integer] -> a
decrypt key = read . map (chr . fromInteger . decryptNumber key)
