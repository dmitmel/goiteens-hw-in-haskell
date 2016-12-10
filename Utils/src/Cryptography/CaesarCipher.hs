module Cryptography.CaesarCipher
( encode, decode
, encodeROT13, decodeROT13
) where

import Cryptography

encode :: Int -> Alphabet -> String -> String
encode = shift

decode :: Int -> Alphabet -> String -> String
decode s = shift (-s)

encodeROT13 :: String -> String
encodeROT13 = encode 13 english

decodeROT13 :: String -> String
decodeROT13 = decode 13 english
