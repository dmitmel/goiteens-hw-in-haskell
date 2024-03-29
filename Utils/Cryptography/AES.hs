module Cryptography.AES where

import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString.Lazy.UTF8 as B
import           Data.Bits
import           Data.List.Utils
import           Data.Table
import           Data.Word

type Byte        = Word8
type Block       = [Byte]
type Key         = Block
type State       = Table Byte
type KeySchedule = Table Byte

splitToBlocks :: B.ByteString -> [Block]
splitToBlocks = toBlocks 16 . B.unpack

sbox :: [Word8]
sbox = [ 0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76
       , 0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0
       , 0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15
       , 0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75
       , 0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84
       , 0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf
       , 0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8
       , 0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2
       , 0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73
       , 0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb
       , 0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79
       , 0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08
       , 0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a
       , 0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e
       , 0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf
       , 0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
       ]

rcon :: [[Word8]]
rcon = [ [0x00, 0x00, 0x00, 0x00]
       , [0x01, 0x00, 0x00, 0x00]
       , [0x02, 0x00, 0x00, 0x00]
       , [0x04, 0x00, 0x00, 0x00]
       , [0x08, 0x00, 0x00, 0x00]
       , [0x10, 0x00, 0x00, 0x00]
       , [0x20, 0x00, 0x00, 0x00]
       , [0x40, 0x00, 0x00, 0x00]
       , [0x80, 0x00, 0x00, 0x00]
       , [0x1b, 0x00, 0x00, 0x00]
       , [0x36, 0x00, 0x00, 0x00]
       ]


toState :: Block -> State
toState = toTableVerticalDefault 4 0

fromState :: State -> Block
fromState = concat . tableToCols

subBytes :: State -> State
subBytes = fmap (\byte -> sbox !! fromIntegral byte)

shiftRows :: State -> State
shiftRows (Table rows_ cols_ grid_) = Table rows_ cols_ $ zipWith rotateLeft [0..] grid_

mixColumns :: State -> State
mixColumns state = foldl (\state_ (col, colValues) -> state_ #|= (col, colValues)) state $ map (\col -> (col, computeColumn col)) [0..3]
    where
        computeColumn :: Int -> [Word8]
        computeColumn col = [ (state # (0, col)) &* 2 &+ (state # (1, col)) &* 3 &+ (state # (2, col)) &* 1 &+ (state # (3, col)) &* 1
                            , (state # (0, col)) &* 1 &+ (state # (1, col)) &* 2 &+ (state # (2, col)) &* 3 &+ (state # (3, col)) &* 1
                            , (state # (0, col)) &* 1 &+ (state # (1, col)) &* 1 &+ (state # (2, col)) &* 2 &+ (state # (3, col)) &* 3
                            , (state # (0, col)) &* 3 &+ (state # (1, col)) &* 1 &+ (state # (2, col)) &* 1 &+ (state # (3, col)) &* 2
                            ]

addRoundKey :: KeySchedule -> Int -> State -> State
addRoundKey schedule round_ state = foldl (\state_ col ->
    state_ #= ((0, col), state # (0, col) `xor` schedule # (0, 4 * round_ + col))
           #= ((1, col), state # (1, col) `xor` schedule # (1, 4 * round_ + col))
           #= ((2, col), state # (2, col) `xor` schedule # (2, 4 * round_ + col))
           #= ((3, col), state # (3, col) `xor` schedule # (3, 4 * round_ + col)) ) state [0..3]

keyExpansion :: Key -> KeySchedule
keyExpansion key = foldl (\schedule col -> if col `mod` 4 == 0 then fillMultipleOf4 col schedule else fillColumn col schedule) firstSchedule [4..43]
    where
        firstSchedule = toTableVerticalDefault 4 0x01 key #|+ (40, 0)

        fillMultipleOf4 :: Int -> KeySchedule -> KeySchedule
        fillMultipleOf4 col schedule = schedule #|= (col, zipWith3 (\a b c -> a `xor` b `xor` c) (schedule #| (col - 4)) modifiedPrevCol (rcon !! (col `div` 4 - 1)))
            where
                modifiedPrevCol = fmap (\byte -> sbox !! fromIntegral byte) $ rotateLeft 1 $ schedule #| (col - 1)

        fillColumn :: Int -> KeySchedule -> KeySchedule
        fillColumn col schedule = schedule #|= (col, zipWith xor (schedule #| (col - 1)) (schedule #| (col - 4)))

infixl 6 &+
(&+) :: Word8 -> Word8 -> Word8
(&+) = xor

infixl 7 &*
(&*) :: Word8 -> Word8 -> Word8
num &* 1 = num
num &* 2
    | num < 0x80 = fromIntegral $  (num16 `shiftL` 1)             `mod` 0x100
    | otherwise  = fromIntegral $ ((num16 `shiftL` 1) `xor` 0x1b) `mod` 0x100
    where num16 = fromIntegral num :: Word16
num &* 3 = num &* 2 &+ num
_   &* m = error $ "Cryptography.AES.(&*): Unsupported multiplier: " ++ show m

encryptBlock :: Key -> Block -> Block
encryptBlock key block
    | length key > 16  = error $ "Cryptography.AES.encrypt: too long key (" ++ show (length key) ++ ")"
    | null key         = error   "Cryptography.AES.encrypt: too small key (0)"
    | otherwise =
        let initialState    = addRoundKey keySchedule 0 $ toState block
            keySchedule     = keyExpansion key
            lastButOneState = foldl (\state round_ -> addRoundKey keySchedule round_ $ mixColumns $ shiftRows $ subBytes state) initialState [1..9]
        in fromState $ addRoundKey keySchedule 10 $ shiftRows $ subBytes lastButOneState
