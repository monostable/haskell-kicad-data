import System.Environment
import System.Exit
import System.IO
import Codec.Text.Detect (detectEncodingName)
import Data.Maybe (fromMaybe)
import Data.Encoding
import qualified Data.ByteString.Lazy as L

import qualified Data.Kicad.PcbnewExpr as PcbnewExpr

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "invalid argument\nUSAGE: ./Parse [FILES]"
        fs -> parseAndDisplay fs

parseAndDisplay :: [String] -> IO ()
parseAndDisplay [] = return ()
parseAndDisplay (f:fs) = do
            input <- L.readFile f
            let name = detectEncodingName input
                enc  = encodingFromString (fromMaybe "" name)
                str  = decodeLazyByteString enc input
            case PcbnewExpr.parseWithFilename f str of
                Left err -> hPutStrLn stderr err >> exitFailure
                Right px -> print (PcbnewExpr.pretty px) >> parseAndDisplay fs
