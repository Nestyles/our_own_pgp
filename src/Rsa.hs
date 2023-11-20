module Rsa
    ( getCoprimeList,
      getE,
      getD,
      rsaCryption,
      rsa,
      phi
      -- getLastDivider,
      -- getPrimeFactor
    ) where
import Prelude

isPrimeLoop :: Int -> Int -> Int -> Bool
isPrimeLoop i maxI n
  | i > maxI = True
  | n `mod` i == 0 = False
  | otherwise = isPrimeLoop (i + 1) maxI n

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = isPrimeLoop 2 (isqrt n) n

phi :: Int -> Int -> Int
phi p q = (p - 1) * (q - 1)

-- getLastDivider :: Int -> Int -> Int
-- getLastDivider n i
--   | (n `mod` i) == 0 = getLastDivider (n `div` i) i
--   | otherwise = n

-- getPrimeFactor :: Int -> Int -> [Int]
-- getPrimeFactor n i
--   | (i * i) >= n && n > 1 = [n]
--   | (i * i) >= n = []
--   | (n `mod` i) == 0 = i : getPrimeFactor (getLastDivider n i) (i + 1)
--   | otherwise = getPrimeFactor n (i + 1)

getCoprimeList :: Int -> Int -> [Int]
getCoprimeList x i
  | x == i = []
  | gcd x i == 1 = i : getCoprimeList x (i + 1)
  | otherwise = getCoprimeList x (i + 1)

getE :: [Int] -> Int -> Int
getE [] _ = -1
getE [x] _ = x
getE (x:xs) i
  | gcd x i == 1 = x
  | otherwise = getE xs i

getD :: Int -> Int -> Int -> Int
getD d e p
  | (d * e) `mod` p == 1 = d
  | otherwise = getD (d + 1) e p

rsaCryption :: (Int, Int) -> Int -> Maybe Int
rsaCryption (_, b) x
  | x >= b = Nothing
rsaCryption (a, b) x = Just ((x ^ a) `mod` b)

rsa :: Int -> Int -> ((Int, Int), (Int, Int))
rsa a b = do
  let n = a * b
  let p = phi a b
  let coprimeList = getCoprimeList n 2
  let e = getE coprimeList p
  let d = getD 0 e p
  ((e, n), (d, n))
