module Cryptography.VigenereCipher
( encode, decode
) where

import           Cryptography

encode :: String -> Alphabet -> String -> String
encode passphrase alphabet str = map (\(c, s) -> shiftChar s alphabet c) $ zipWithShifts alphabet str $ passphraseShifts alphabet passphrase

passphraseShifts :: Alphabet -> String -> [Int]
passphraseShifts alphabet = map (alphabetOrd alphabet)

zipWithShifts :: Alphabet -> String -> [Int] -> [(Char, Int)]
zipWithShifts alphabet str passphraseShifts_ = reverse $ snd $ foldl (\(shifts, withShifts) c ->
    if c `alphabetElem` alphabet
        then if null shifts
            then (tail passphraseShifts_, (c, head passphraseShifts_) : withShifts)
            else (tail shifts,            (c, head shifts)            : withShifts)
        else (shifts, (c, 0) : withShifts)) (passphraseShifts_, []) str

decode :: String -> Alphabet -> String -> String
decode passphrase alphabet str = map (\(c, s) -> shiftChar (-s) alphabet c) $ zipWithShifts alphabet str $ passphraseShifts alphabet passphrase
