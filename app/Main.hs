module Main (main) where

import Lib
import System.Environment (getArgs)
import Args (parseArgs, handleError, defaultArgs)

main :: IO ()
main = do
    args <- getArgs
    let parsedArgs = parseArgs args defaultArgs
    print "Hello"
