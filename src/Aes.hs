module Aes (encryptAes, decryptAes) where

import Matrix (Matrix, Vector)
import Lookup (mul3, mul2, sboxRev, sbox, mul14, mul11, mul13, mul9, rcon)
import Data.Bits (Bits(xor))
import Control.Exception (Exception, throw)
import Data.Word (Word8)

data AesException
  = WrongMix
    | WrongRcon
    | WrongExpansion
  deriving (Show)

instance Exception AesException

subVecBytes :: Vector -> Vector
subVecBytes = map sbox

subBytes :: Matrix -> Matrix
subBytes = map subVecBytes

subVecBytesRev :: Vector -> Vector
subVecBytesRev = map sboxRev

subBytesRev :: Matrix -> Matrix
subBytesRev = map subVecBytesRev

shiftLeft :: Int -> Vector -> Vector
shiftLeft n xs = drop n xs ++ take n xs

shiftRight :: Int -> Vector -> Vector
shiftRight n xs = drop len xs ++ take len xs
    where
        len = length xs - n

-- shiftRows :: [[a]] -> [[a]]
-- shiftRows state = zipWith shiftRow [0..3] state
--   where
--     shiftRow i row = take 4 $ drop i (cycle row)

rotateRows :: Matrix -> Matrix
rotateRows
    [[i00, i01, i02, i03],
    [i04, i05, i06, i07],
    [i08, i09, i10, i11],
    [i12, i13, i14, i15]] = [
        [i00, i05, i10, i15],
        [i04, i09, i14, i03],
        [i08, i13, i02, i07],
        [i12, i01, i06, i11]
    ]
rotateRows _ = throw WrongMix

rotateRowsInv :: Matrix -> Matrix
rotateRowsInv
    [[i00, i05, i10, i15],
    [i04, i09, i14, i03],
    [i08, i13, i02, i07],
    [i12, i01, i06, i11]] = [
        [i00, i01, i02, i03],
        [i04, i05, i06, i07],
        [i08, i09, i10, i11],
        [i12, i13, i14, i15]
    ]
rotateRowsInv _ = throw WrongMix

shiftRows :: Matrix -> Matrix
shiftRows = zipWith shiftLeft [0..]

shiftRowsInv :: Matrix -> Matrix
shiftRowsInv = zipWith shiftRight [0..]

-- fonction maudite https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bits.html
-- gmul :: Word8 -> Word8 -> Word8
-- gmul a b = gOp 0 8
--     where
--         gOp :: Word8 -> Word8 -> Word8
--         gOp p 0 = p
--         gOp p counter =
--             let p' = if b .&. 1 /= 0 then p `xor` a else p
--                 hi_bit_set = a .&. 0x80 -- 
--                 a' = if hi_bit_set /= 0 then (a `shiftL` 1) `xor` 0x1b else a `shiftL` 1
--                 b' = b `shiftR` 1 in
--                 gOp p' (counter - 1)

mixColumns :: Matrix -> Matrix
mixColumns
    [[i00, i01, i02, i03],
    [i04, i05, i06, i07],
    [i08, i09, i10, i11],
    [i12, i13, i14, i15]] =
    [[
        mul2 i00 `xor` mul3 i01 `xor` i02 `xor` i03,
        i00 `xor` mul2 i01 `xor` mul3 i02 `xor` i03,
        i00 `xor` i01 `xor` mul2 i02 `xor` mul3 i03,
        mul3 i00 `xor` i01 `xor` i02 `xor` mul2 i03
    ],
    [
        mul2 i04 `xor` mul3 i05 `xor` i06 `xor` i07,
        i04 `xor` mul2 i05 `xor` mul3 i06 `xor` i07,
        i04 `xor` i05 `xor` mul2 i06 `xor` mul3 i07,
        mul3 i04 `xor` i05 `xor` i06 `xor` mul2 i07
    ],
    [
        mul2 i08 `xor` mul3 i09 `xor` i10 `xor` i11,
        i08 `xor` mul2 i09 `xor` mul3 i10 `xor` i11,
        i08 `xor` i09 `xor` mul2 i10 `xor` mul3 i11,
        mul3 i08 `xor` i09 `xor` i10 `xor` mul2 i11
    ],
    [
        mul2 i12 `xor` mul3 i13 `xor` i14 `xor` i15,
        i12 `xor` mul2 i13 `xor` mul3 i14 `xor` i15,
        i12 `xor` i13 `xor` mul2 i14 `xor` mul3 i15,
        mul3 i12 `xor` i13 `xor` i14 `xor` mul2 i15
    ]]
mixColumns _ = throw WrongMix

mixColumnsInv :: Matrix -> Matrix
mixColumnsInv
    [[i00, i01, i02, i03],
    [i04, i05, i06, i07],
    [i08, i09, i10, i11],
    [i12, i13, i14, i15]] =
    [[
        mul14 i00 `xor` mul11 i01 `xor` mul13 i02 `xor` mul9 i03,
        mul9 i00 `xor` mul14 i01 `xor` mul11 i02 `xor` mul13 i03,
        mul13 i00 `xor` mul9 i01 `xor` mul14 i02 `xor` mul11 i03,
        mul11 i00 `xor` mul13 i01 `xor` mul9 i02 `xor` mul14 i03
    ],
    [
        mul14 i04 `xor` mul11 i05 `xor` mul13 i06 `xor` mul9 i07,
        mul9 i04 `xor` mul14 i05 `xor` mul11 i06 `xor` mul13 i07,
        mul13 i04 `xor` mul9 i05 `xor` mul14 i06 `xor` mul11 i07,
        mul11 i04 `xor` mul13 i05 `xor` mul9 i06 `xor` mul14 i07
    ],
    [
        mul14 i08 `xor` mul11 i09 `xor` mul13 i10 `xor` mul9 i11,
        mul9 i08 `xor` mul14 i09 `xor` mul11 i10 `xor` mul13 i11,
        mul13 i08 `xor` mul9 i09 `xor` mul14 i10 `xor` mul11 i11,
        mul11 i08 `xor` mul13 i09 `xor` mul9 i10 `xor` mul14 i11
    ],
    [
        mul14 i12 `xor` mul11 i13 `xor` mul13 i14 `xor` mul9 i15,
        mul9 i12 `xor` mul14 i13 `xor` mul11 i14 `xor` mul13 i15,
        mul13 i12 `xor` mul9 i13 `xor` mul14 i14 `xor` mul11 i15,
        mul11 i12 `xor` mul13 i13 `xor` mul9 i14 `xor` mul14 i15
    ]]
mixColumnsInv _ = throw WrongMix

xorColumn :: Vector -> Vector -> Vector
xorColumn = zipWith xor

applyRcon :: Word8 -> Vector -> Vector
applyRcon i [o, t, th, f] = [o `xor` rcon i, t, th, f]
applyRcon _ _ = throw WrongRcon

keyExpansionCore :: Word8 -> Vector -> Vector
keyExpansionCore i m = rc
    where
        shifted = shiftLeft 1 m
        sboxed = subVecBytes shifted
        rc = applyRcon i sboxed

keyExpansionLoop :: Word8 -> Matrix -> Matrix
keyExpansionLoop i m = [w4, w5, w6, w7]
    where
        firstVec :: Vector
        firstVec = head m
        w4 = firstVec `xorColumn` keyExpansionCore i (last m)
        w5 = w4 `xorColumn` (m !! 1)
        w6 = w5 `xorColumn` (m !! 2)
        w7 = w6 `xorColumn` (m !! 3)

keyExpansion :: Matrix -> [Matrix]
keyExpansion m = m : looper 1 m
    where
        looper :: Word8 -> Matrix -> [Matrix]
        looper 11 _ = []
        looper i ma = new : looper (i + 1) new
            where
                new = keyExpansionLoop i ma

addRoundKey :: Matrix -> Matrix -> Matrix
addRoundKey = zipWith xorColumn

playOneEncRound :: Matrix -> Matrix -> Matrix
playOneEncRound key msg = add
    where
        sub = subBytes msg
        shift = rotateRows sub
        mix = mixColumns shift
        add = addRoundKey mix key

encryptAes :: Matrix -> Matrix -> Matrix
encryptAes key msg = lastRound
    where
        expandedKey = keyExpansion key
        firstAdd = addRoundKey (head expandedKey) msg
        afterRounds = looper 1 firstAdd expandedKey
        lastRound = addRoundKey (expandedKey !! 10) $ rotateRows $ subBytes afterRounds
        looper :: Int -> Matrix -> [Matrix] -> Matrix
        looper 10 m _ = m
        looper i m keys = looper (i + 1) (playOneEncRound (keys !! i) m) expandedKey

playOneDecRound :: Matrix -> Matrix -> Matrix
playOneDecRound key msg = mix
    where
        shift = rotateRowsInv msg
        sub = subBytesRev shift
        add = addRoundKey sub key
        mix = mixColumnsInv add

decryptAes :: Matrix -> Matrix -> Matrix
decryptAes key msg = lastRound
    where
        expandedKey = reverse $ keyExpansion key
        firstAdd = addRoundKey (head expandedKey) msg
        afterRounds = looper 1 firstAdd expandedKey
        lastRound = addRoundKey (expandedKey !! 10) $ subBytesRev $ rotateRowsInv afterRounds
        looper :: Int -> Matrix -> [Matrix] -> Matrix
        looper 10 m _ = m
        looper i m keys = looper (i + 1) (playOneDecRound (keys !! i) m) expandedKey