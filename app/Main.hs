{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Environment (getArgs)
import Args (parseArgs, defaultArgs, hexToArr, Args (..), checkBlock)
import Matrix (getMatrix)
import Aes (encryptAes, decryptAes)
import Xor (xorKey)
import Data.Word (Word8)
import Text.Printf (printf)
import System.Random

generateKey :: IO [Word8]
generateKey = do
  gen <- newStdGen
  let keyBytes = take 16 $ randoms gen :: [Word8]
  return keyBytes

printMatrix :: [[Word8]] -> IO ()
printMatrix = mapM_ $ mapM_ (printf "%02x")

convertStreamMode :: [Word8] -> [Word8] -> Int -> [[Word8]]
convertStreamMode _ [] _ = []
convertStreamMode key message keySize = padded : convertStreamMode key remaining keySize
    where
        (chunk, remaining) = splitAt keySize message
        padded = chunk ++ replicate (keySize - length chunk) 0

getArgsKey :: Args -> [Word8]
getArgsKey (Args _ _ _ _ _ (Just key)) = key
getArgsKey _ = []

playStreamMode :: Args -> [Word8] -> IO ()
playStreamMode args message = mapM_ (chooseInstruction args) messageStream
    where
        key = getArgsKey args
        messageStream = convertStreamMode key message (length key)

chooseInstruction :: Args -> [Word8] -> IO ()
chooseInstruction (Args (Just "aes") (Just True) _ Nothing _ (Just key)) msg = printMatrix $ encryptAes (getMatrix key 4) (getMatrix msg 4)
chooseInstruction (Args (Just "aes") (Just False) _ Nothing _ (Just key)) msg = printMatrix $ decryptAes (getMatrix key 4) (getMatrix msg 4)
chooseInstruction (Args (Just "xor") _ block Nothing _ (Just key)) msg = mapM_ (printf "%02x") $ xorKey key msg msg

main :: IO ()
main = do
    args <- getArgs
    let !parsedArgs = parseArgs args defaultArgs -- ! = bang pattern to force evaluation
    hexMessage <- getLine
    checkBlock parsedArgs (hexToArr hexMessage)
    playStreamMode parsedArgs (hexToArr hexMessage)