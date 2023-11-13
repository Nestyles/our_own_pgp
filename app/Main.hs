module Main (main) where

import System.Environment (getArgs)
import Args (parseArgs, defaultArgs, hexToArr, Args (..))
import Matrix (getMatrix)
import Aes (playRound)
import Lib (xorKey)
import Data.Word (Word8)
import Text.Printf (printf)

chooseInstruction :: Args -> [Word8] -> IO ()
chooseInstruction (Args (Just "aes") (Just clear) block Nothing (Just key)) msg = print $ playRound (getMatrix key 4) msg
chooseInstruction (Args (Just "xor") _ block Nothing (Just key)) msg = mapM_ (printf "%02x") $ xorKey key msg msg

main :: IO ()
main = do
    args <- getArgs
    let parsedArgs = parseArgs args defaultArgs
    hexMessage <- getLine
    chooseInstruction parsedArgs (hexToArr hexMessage)
