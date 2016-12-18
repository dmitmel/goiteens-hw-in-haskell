module Cryptography.RSA.Random where

import           Math.NumberTheory.Primes
import           System.Random

randomWithBits :: RandomGen g => Int -> g -> (Integer, g)
randomWithBits 0 _       = error "Cryptography.RSA.Random.randomWithBits: bits count is too small (0)"
randomWithBits 1 gen     = randomR (0, 1) gen
randomWithBits count gen = randomR (2 ^ (count - 1), 2 ^ count - 1) gen

randomPrime :: RandomGen g => Int -> g -> (Integer, g)
randomPrime bitsCount gen
    | isPrime randomNum = (randomNum, newGen)
    | otherwise         = randomPrime bitsCount newGen
    where (randomNum, newGen) = randomWithBits bitsCount gen
