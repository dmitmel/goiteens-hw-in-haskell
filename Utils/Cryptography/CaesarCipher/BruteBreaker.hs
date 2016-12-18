module Cryptography.CaesarCipher.BruteBreaker
( bruteBreak
, bruteBreakIO
) where

import           Cryptography
import           Cryptography.CaesarCipher

bruteBreak :: Alphabet -> String -> [(Int, String)]
bruteBreak alphabet str = zip allPossibleKeys (map (\s -> decode s alphabet str) allPossibleKeys)
    where allPossibleKeys = [1..alphabetLength alphabet]

bruteBreakIO :: Alphabet -> String -> IO ()
bruteBreakIO alphabet str = mapM_ (\(key, result) -> putStrLn $ show key ++ ": " ++ result) $ bruteBreak alphabet str
