module Main (main) where

import System.Environment (getArgs)
import Args (parseArgs, defaultArgs)

main :: IO ()
main = do
    args <- getArgs
    let parsedArgs = parseArgs args defaultArgs
    putStrLn "Message:"
    hex1 <- getLine
    putStrLn hex1
