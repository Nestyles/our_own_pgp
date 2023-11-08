module Args (
    parseArgs, defaultArgs, handleError
    ) where
;
import Control.Exception
import System.Exit (exitWith, ExitCode (ExitFailure))
import Data.Int ( Int8 )
import Numeric (readHex)

data ExceptionType = WrongArg
                | MissingRule
                | WrongFormat
                deriving (Show)

instance Exception ExceptionType

type CompType = String
type IsClear = Bool
type BlockMode = Bool
type RsaOpts = (Int, Int)
type Key = [Int8]

data Args = Args (Maybe CompType) (Maybe IsClear) (Maybe BlockMode) (Maybe RsaOpts) (Maybe Key)

defaultArgs :: Args
defaultArgs = Args Nothing Nothing Nothing Nothing Nothing

handleError :: ExceptionType -> IO ()
handleError WrongArg = putStrLn "Wrong argument format" >>
    exitWith (ExitFailure 84)
handleError MissingRule = putStrLn "You need to specify all arguments"
    >> exitWith (ExitFailure 84)
handleError WrongFormat = putStrLn "The file doesn't have the right format"
    >> exitWith (ExitFailure 84)

splitStringByByte :: String -> [String]
splitStringByByte (x : y : xs) = [x, y] : splitStringByByte xs
splitStringByByte [] = []
splitStringByByte _ = throw WrongFormat

pairToByte :: String -> Int8
pairToByte b = case readHex b of
    [(n, "")] -> n
    _ -> throw WrongArg

hexToArr :: String -> [Int8]
hexToArr s = map pairToByte $ splitStringByByte s

parseArg :: Args -> String -> Args
parseArg (Args Nothing c p r k) "-xor" = Args (Just "xor") c p r k
parseArg (Args Nothing c p r k) "-aes" = Args (Just "aes") c p r k
parseArg (Args Nothing c p r k) "-rsa" = Args (Just "rsa") c p r k
parseArg (Args n Nothing p r k) "-c" = Args n (Just True) p r k
parseArg (Args n Nothing p r k) "-d" = Args n (Just False) p r k
parseArg (Args n c Nothing r k) "-b" = Args n c (Just True) r k
parseArg _ _ = throw WrongArg

parseArgs :: [String] -> Args -> Args
parseArgs ("-g": pr: q: xs) (Args n c p _ k) = parseArgs xs (Args n c p (Just (read pr :: Int, read q :: Int)) k)
parseArgs [x] (Args (Just n) (Just c) (Just p) r Nothing) = Args (Just n) (Just c) (Just p) r (Just (hexToArr x))
parseArgs (x: xs) arg = parseArgs xs (parseArg arg x)
parseArgs _ (Args Nothing _ _ _ _) = throw MissingRule
parseArgs _ (Args _ Nothing _ _ _) = throw MissingRule
parseArgs _ (Args _ _ Nothing _ _) = throw MissingRule
parseArgs _ (Args (Just "rsa") _ _ Nothing _) = throw MissingRule
parseArgs _ arg = arg