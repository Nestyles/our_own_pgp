module Xor
    ( xorKey,
    ) where
import Data.Bits ( Bits(xor) )
import Data.Word (Word8)

type Hexa = [Word8]

xorKey :: Hexa -> Hexa -> Hexa -> Hexa
xorKey [x] [y] _ = [xor x y]
xorKey (x:xs) [y] z = xor x y : xorKey xs z z
xorKey (x1:xs1) (x2:xs2) z = xor x1 x2 : xorKey xs1 xs2 z
