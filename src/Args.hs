module Args (
    parseArgs, defaultArgs, checkBlock, handleError, hexToArr, Args (..)
    ) where
;
import Control.Exception
import System.Exit (exitWith, ExitCode (ExitFailure))
import Numeric (readHex)
import Data.Word (Word8)
import System.IO (hPutStrLn, stderr)

data ExceptionType = WrongArg
                | MissingRule
                | WrongBlock
                | WrongPairToByte
                | WrongSplitString
                deriving (Show)

instance Exception ExceptionType

type CompType = String
type IsClear = Bool
type BlockMode = Bool
type RsaOpts = (Int, Int)
type RsaKeys = ([Word8], [Word8])
type Key = [Word8]

data Args = Args (Maybe CompType) (Maybe IsClear) (Maybe BlockMode) (Maybe RsaOpts) (Maybe RsaKeys) (Maybe Key)

defaultArgs :: Args
defaultArgs = Args Nothing Nothing Nothing Nothing Nothing Nothing

handleError :: ExceptionType -> IO ()
handleError WrongArg = hPutStrLn stderr "Wrong argument format" >>
    exitWith (ExitFailure 84)
handleError MissingRule = hPutStrLn stderr "You need to specify all arguments"
    >> exitWith (ExitFailure 84)
handleError WrongBlock = hPutStrLn stderr "You specified BlockMode but wrong input"
    >> exitWith (ExitFailure 84)

checkBlock :: Args -> [Word8] -> IO ()
checkBlock (Args _ _ Nothing _ _ _) _ = return ()
checkBlock (Args _ _ _ _ _ (Just key)) msg
    | length key == length msg = return ()
    | otherwise = throw WrongBlock
checkBlock _ _ = return ()

splitStringByByte :: String -> [String]
splitStringByByte (x : y : xs) = [x, y] : splitStringByByte xs
splitStringByByte [] = []
splitStringByByte _ = throw WrongSplitString

pairToByte :: String -> Word8
pairToByte b = case readHex b of
    [(n, "")] -> n
    _ -> throw WrongSplitString

hexToArr :: String -> [Word8]
hexToArr s = map pairToByte $ splitStringByByte s

parseArg :: Args -> String -> Args
parseArg (Args Nothing c p r rk k) "-xor" = Args (Just "xor") c p r rk k
parseArg (Args Nothing c p r rk k) "-aes" = Args (Just "aes") c p r rk k
parseArg (Args Nothing c p r rk k) "-rsa" = Args (Just "rsa") c p r rk k
parseArg (Args n Nothing p r rk k) "-c" = Args n (Just True) p r rk k
parseArg (Args n Nothing p r rk k) "-d" = Args n (Just False) p r rk k
parseArg (Args n c Nothing r rk k) "-b" = Args n c (Just True) r rk k
parseArg _ _ = throw WrongArg

parseArgs :: [String] -> Args -> Args
parseArgs ("-g": pr: q: xs) (Args n c p _ rk k) = parseArgs xs (Args n c p (Just (read pr :: Int, read q :: Int)) rk k)
parseArgs [x] (Args (Just n) (Just c) b r Nothing Nothing) = Args (Just n) (Just c) b r Nothing (Just (hexToArr x)) -- Should parse rsa keys instead of Nothing
parseArgs [x] (Args (Just n) (Just c) b r rk Nothing) = Args (Just n) (Just c) b r rk (Just (hexToArr x))
parseArgs (x: xs) arg = parseArgs xs (parseArg arg x)
parseArgs _ (Args Nothing _ _ _ _ _ ) = throw MissingRule
parseArgs _ (Args _ Nothing _ _ _ _ ) = throw MissingRule
parseArgs _ (Args (Just "rsa") _ _ Nothing  _ _) = throw MissingRule
parseArgs _ arg = arg