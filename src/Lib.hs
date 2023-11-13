module Lib
    ( someFunc
    ) where
import Data.Bits ( Bits(xor) )
import Data.Word (Word8)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Hexa = [Word8]

reverseKey :: Hexa -> Hexa
reverseKey = reverse

xorKey :: Hexa -> Hexa -> Hexa -> Hexa
xorKey  [x] [y] _ = [xor x y]
xorKey (x:xs) [y] z = xor x y:xorKey xs z z
xorKey (x1:xs1) (x2:xs2) z = xor x1 x2:xorKey xs1 xs2 z
