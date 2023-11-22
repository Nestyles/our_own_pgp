{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Environment (getArgs)
import Args (parseArgs, defaultArgs, hexToArr, Args (..), checkBlock)
import Matrix (getMatrix)
import Aes (encryptAes, decryptAes)
import Xor (xorKey)
import Data.Word (Word8)
import Text.Printf (printf)

printMatrix :: [[Word8]] -> IO ()
printMatrix = mapM_ $ mapM_ (printf "%02x")

chooseInstruction :: Args -> [Word8] -> IO ()
chooseInstruction (Args (Just "aes") (Just True) block Nothing (Just key)) msg = printMatrix $ encryptAes (getMatrix key 4) (getMatrix msg 4)
chooseInstruction (Args (Just "aes") (Just False) block Nothing (Just key)) msg = printMatrix $ decryptAes (getMatrix key 4) (getMatrix msg 4)
chooseInstruction (Args (Just "xor") _ block Nothing (Just key)) msg = mapM_ (printf "%02x") $ xorKey key msg msg

main :: IO ()
main = do
    args <- getArgs
    let !parsedArgs = parseArgs args defaultArgs -- ! = bang pattern to force evaluation
    hexMessage <- getLine
    checkBlock parsedArgs (hexToArr hexMessage)
    chooseInstruction parsedArgs (hexToArr hexMessage)