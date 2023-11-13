module Main (main) where

import System.Environment (getArgs)
import Args (parseArgs, defaultArgs, hexToArr, Args (..))
import Matrix (getMatrix)

chooseInstruction :: Args -> String -> IO ()
chooseInstruction (Args (Just "aes") (Just clear) block Nothing (Just key)) _ = print $ getMatrix key 4

main :: IO ()
main = do
    args <- getArgs
    let parsedArgs = parseArgs args defaultArgs
    putStrLn "Message:"
    hexMessage <- getLine
    chooseInstruction parsedArgs hexMessage
