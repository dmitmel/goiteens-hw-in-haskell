module Cryptography.CaesarCipher.BruteBreaker
( bruteBreak
, bruteBreakIO
) where

import Cryptography
import Cryptography.CaesarCipher

bruteBreak :: Alphabet -> String -> [String]
bruteBreak alphabet str = map (\s -> decode s alphabet str) [1..alphabetLength alphabet]

bruteBreakIO :: Alphabet -> String -> IO ()
bruteBreakIO alphabet str = mapM_ putStrLn $ bruteBreak alphabet str
