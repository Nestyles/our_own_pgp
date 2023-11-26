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
      stringToHex
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

getD :: Integer -> Integer -> Integer
getD a b = d
  where
    (_, d, _) = egcd a b

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

-- modpow :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
-- modpow b e m c e'
--   | e' < e = modpow b e m newC (e' + 1)
--   | otherwise = newC
--     where
--       newC = (b * c) `mod` m

rsaCryption :: RsaKeys -> Integer -> Integer
rsaCryption (_, n) m
  | m >= n = throw WrongMessage
  | m < 0 = throw WrongMessage
rsaCryption (e, n) m = (m ^ e) `mod` n

rsa :: Integer -> Integer -> (RsaKeys, RsaKeys)
rsa p q = do
  let n = getN p q
  let ph = phi p q
  let e = getE 65537 ph
  let d = getD e ph
  ((e, n), (d, n))

printRsa :: (RsaKeys, RsaKeys) -> IO ()
printRsa ((e, n1), (d, n2)) = printf "public key: %s-%s\nprivate key: %s-%s\n" (hexToString e) (hexToString n1) (hexToString d) (hexToString n2)
