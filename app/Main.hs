module Main (main) where

import System.Environment (getArgs)
import Args (parseArgs, defaultArgs, hexToArr)

main :: IO ()
main = do
    args <- getArgs
    let parsedArgs = parseArgs args defaultArgs
    putStrLn "Message:"
    hexMessage <- getLine
    print $ hexToArr hexMessage
