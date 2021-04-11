module Math.MoreMath.BigInt
( BigInt(bigIntBits)
, bigIntLength
, showBits
, toBigInt
, fromBigInt
) where

import Data.Bits
import Data.Monoid ((<>))

type BitArray = [Bool]

data BigInt = BigInt { bigIntBits :: BitArray } deriving (Eq)

b1 :: Bool
b1 = True
b0 :: Bool
b0 = False

instance Bits BigInt where
    (BigInt a) .&. (BigInt b) = BigInt $ zipWith (\bitA bitB -> if bitA && bitB then b1 else b0) a b
    (BigInt a) .|. (BigInt b) = BigInt $ zipWith (\bitA bitB -> if bitA || bitB then b1 else b0) a b

    (BigInt a) `xor` (BigInt b) = BigInt $ zipWith (\bitA bitB -> if bitA /= bitB then b1 else b0) a b

    complement (BigInt bs) = BigInt $ map not bs

    shiftL (BigInt bs) s = BigInt $ bs <<| s
    shiftR (BigInt bs) s = BigInt $ bs |>> s

    rotateL = shiftL
    rotateR = shiftR

    zeroBits = BigInt [b0]
    bit = bitDefault

    (BigInt bs) `setBit` b
        | b >= length bs = BigInt $ bs ++ replicate (b - length bs) b0 ++ [b1]
        | otherwise      = BigInt $ take b bs ++ [b1] ++ drop (b + 1) bs
    (BigInt bs) `clearBit` b
        | b >= length bs = BigInt bs
        | otherwise      = BigInt $ take b bs ++ [b0] ++ drop (b + 1) bs
    (BigInt bs) `complementBit` b
        | b >= length bs = BigInt $ bs ++ replicate (b - length bs) b0 ++ [b1]
        | otherwise      = BigInt $ take b bs ++ (if bs !! b then [b1] else [b0]) ++ drop (b + 1) bs
    (BigInt bs) `testBit` b
        | b >= length bs = False
        | otherwise      = bs !! b

    bitSizeMaybe _ = Nothing
    bitSize      _ = undefined

    isSigned  _ = False

    popCount (BigInt bs) = length $ filter (== b1) bs

bigIntLength :: BigInt -> Int
bigIntLength (BigInt bs) = length bs

showBits :: BigInt -> String
showBits (BigInt bs) = reverse $ map (\b -> if b then '1' else '0') bs

instance Ord BigInt where
    a `compare` b = bigIntSignum a `compare` bigIntSignum b
                 <> bigIntLength a `compare` bigIntLength b
                 <> bigIntBits   a `compare` bigIntBits   b
        where
            bigIntSignum (BigInt bs)
                | all (== b0) bs = 0 :: Int
                | otherwise      = 1 :: Int

instance Show BigInt where
    show bigInt = let integer = (fromBigInt bigInt :: Integer) in show integer

instance Enum BigInt where
    succ (BigInt bs) = BigInt $ bitPlus1 bs
    pred (BigInt bs) = BigInt $ bitMinus1 bs

    toEnum   = toBigInt
    fromEnum = fromBigInt

instance Num BigInt where
    (BigInt bitsA) + (BigInt bitsB) = BigInt $ bitsA `bitPlus` bitsB
    (BigInt bitsA) - (BigInt bitsB) = BigInt $ bitsA `bitMinus` bitsB
    (BigInt bitsA) * (BigInt bitsB) = BigInt $ bitsA `bitMultiply` bitsB

    negate _ = error "Math.BigNumbers.BigInt.negate: unsupported operation"

    signum (BigInt bs)
        | all (== b0) bs = 0
        | otherwise      = 1

    fromInteger = toBigInt

    abs = id

toBigInt :: (Num a, Ord a) => a -> BigInt
toBigInt i = BigInt $ fst $ foldr (\m (bin, i_) -> if m <= i_ then (b1:bin, i_ - m) else (b0:bin, i_)) ([], i) $ takeWhile (<= i) $ iterate (*2) 1

fromBigInt :: Num a => BigInt -> a
fromBigInt (BigInt bs) = foldr (\b acc -> acc * 2 + (if b then 1 else 0)) 0 bs

bitPlus1 :: BitArray -> BitArray
bitPlus1 [] = [b1]
bitPlus1 (b:nextBits)
    | b         = b0 : bitPlus1 nextBits
    | otherwise = b1 : nextBits

nullOrDefault :: [a] -> [a] -> [a]
nullOrDefault def [] = def
nullOrDefault _   xs = xs

bitPlus :: BitArray -> BitArray -> BitArray
bitPlus [] b = nullOrDefault [b0] b
bitPlus a [] = nullOrDefault [b0] a
bitPlus (bitA:bitsA) (bitB:bitsB)
    | bitA && bitB = b0 : bitPlus1 (bitPlus bitsA bitsB)
    | bitA || bitB = b1 : bitPlus bitsA bitsB
    | otherwise    = b0 : bitPlus bitsA bitsB

bitMinus1 :: BitArray -> BitArray
bitMinus1 [] = error "Math.BigNumbers.BigInt.bitTake1: no bits to take 1"
bitMinus1 (b:nextBits)
    | b         = b0 : nextBits
    | otherwise = b1 : bitMinus1 nextBits

bitMinus :: BitArray -> BitArray -> BitArray
bitMinus [] [] = [b0]
bitMinus [] _ = error "Math.BigNumbers.BigInt.bitMinus: b is greater than a"
bitMinus a [] = a
bitMinus (bitA:nextBitsA) (bitB:nextBitsB)
    | bitA && bitB     = b0 : bitMinus nextBitsA nextBitsB
    | bitA && not bitB = b1 : bitMinus nextBitsA nextBitsB
    | not bitA && bitB = b1 : bitMinus1 (bitMinus nextBitsA nextBitsB)
    | otherwise        = b0 : bitMinus nextBitsA nextBitsB

infixl 9 <<|

(<<|) :: BitArray -> Int -> BitArray
[] <<| _ = error "Math.BigNumbers.BigInt.(<<|): nothing to shift"
bs <<| 0 = bs
bs <<| s = replicate s b0 ++ bs

infixl 9 |>>

(|>>) :: BitArray -> Int -> BitArray
[] |>> _ = error "Math.BigNumbers.BigInt.(|>>): nothing to shift"
bs |>> 0 = bs
bs |>> s = nullOrDefault [b0] $ drop s bs

bitMultiply :: BitArray -> BitArray -> BitArray
bitMultiply _     [] = [b0]
bitMultiply bitsA (bitB:nextBitsB)
    | bitB      = bitPlus bitsA (bitMultiply bitsA nextBitsB <<| 1)
    | otherwise = bitMultiply bitsA nextBitsB <<| 1

-- bitDivide :: Bits -> Bits -> Bits
-- bitDivide bitsA bitsB
