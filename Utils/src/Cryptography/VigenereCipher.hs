module Cryptography.VigenereCipher
( encode, decode
) where

import Cryptography

encode :: String -> Alphabet -> String -> String
encode passphrase alphabet str = map (\(s, c) -> if c `alphabetElem` alphabet then shiftChar s alphabet c else c) $ zip (map (alphabetOrd alphabet) (cycle passphrase)) str

decode :: String -> Alphabet -> String -> String
decode passphrase alphabet str = map (\(s, c) -> if c `alphabetElem` alphabet then shiftChar (-s) alphabet c else c) $ zip (map (alphabetOrd alphabet) (cycle passphrase)) str
