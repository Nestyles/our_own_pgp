{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use even" #-}
module Rsa
    (
      getE,
      getD,
      egcd,
      rsaCryption,
      rsa,
      phi,
      isPrime,
      printRsa,
      hexToString,
      stringToHex,
      modpow
      -- getLastDivider,
      -- getPrimeFactor
    ) where
import Args(RsaKeys, strToInt)
import Prelude
import Text.Printf (printf)
import Numeric (showHex)
import Control.Exception (Exception, throw)

data RsaException
  = WrongPQ
    | WrongMessage
  deriving (Show)

instance Exception RsaException

isPrimeLoop :: Integer -> Integer -> Integer -> Bool
isPrimeLoop i maxI n
  | i > maxI = True
  | n `mod` i == 0 = False
  | otherwise = isPrimeLoop (i + 1) maxI n

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = isPrimeLoop 2 (isqrt n) n

phi :: Integer -> Integer -> Integer
phi p q = (p - 1) * (q - 1)

getN :: Integer -> Integer -> Integer
getN p q
  | p == q = throw WrongPQ
  | otherwise = p * q

getE :: Integer -> Integer -> Integer
getE e ph
  | e < 20 = getE 20 ph
  | gcd e ph == 1 = e
  | otherwise = getE (e + 1) ph

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = (gcd', x, y)
  where
    (gcd', x1, y1) = egcd (b `mod` a) a
    x = y1 - (b `div` a) * x1
    y = x1

modinv :: Integer -> Integer -> Integer
modinv a m
  | gcd' /= 1 = throw WrongPQ
  | otherwise = d `mod` m
  where
    (gcd', d, _) = egcd a m
  

getD :: Integer -> Integer -> Integer
getD = modinv

reverseString :: String -> String
reverseString [] = []
reverseString [x] = '0': [x]
reverseString [x, y] = [x, y]
reverseString (x: y: xs) = reverseString xs ++ (x : [y])

stringToHex :: String -> Integer
stringToHex str = strToInt reverseStr
    where
        reverseStr = reverseString str

hexToString :: Integer -> String
hexToString hex = reverseString $ showHex hex ""

modpow :: Integer -> Integer -> Integer -> Integer
modpow 0 _ _ = 0
modpow _ 0 _ = 1
modpow a b c
  | b `mod` 2 == 0 = (y' + c) `mod` c
    where
      y = modpow a (b `div` 2) c
      y' = (y * y) `mod` c
modpow a b c
  | b `mod` 2 == 1 = (y' + c) `mod` c
    where
      y = a `mod` c
      y' = (y * modpow a (b - 1) c `mod` c) `mod` c

rsaCryption :: RsaKeys -> Integer -> Integer
rsaCryption (_, n) m
  | m >= n = throw WrongMessage
  | m < 0 = throw WrongMessage
rsaCryption (e, n) m = modpow m e n

rsa :: Integer -> Integer -> (RsaKeys, RsaKeys)
rsa p q = do
  let n = getN p q
  let ph = phi p q
  let e = getE 257 ph
  let d = getD e ph
  ((e, n), (d, n))

printRsa :: (RsaKeys, RsaKeys) -> IO ()
printRsa ((e, n1), (d, n2)) = printf "public key: %s-%s\nprivate key: %s-%s\n" (hexToString e) (hexToString n1) (hexToString d) (hexToString n2)
