import Codec.Text.Detect (detectEncodingName)
import Control.Concurrent.Async (mapConcurrently_)
import Data.Encoding
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing)
import System.Directory.PathWalk (pathWalkLazy)
import System.Environment
import System.Exit
import System.FilePath (takeDirectory, joinPath)
import System.IO
import qualified Data.ByteString.Lazy as L

import qualified Data.Kicad.PcbnewExpr as PcbnewExpr

main :: IO ()
main = do
    args <- getArgs
    case args of
        [folder] -> do
            pw <- pathWalkLazy folder
            let mods = pw >>= (\(root, dirs, files) ->
                        files >>= (\file ->
                            if (".kicad_mod" `isSuffixOf` file) then [joinPath [root, file]] else []))
            putStrLn $ "Running parse on " ++ (show (length mods)) ++ " .kicad_mod files"
            _ <- mapConcurrently_ parseAndWrite mods
            return ()
        _ -> hPutStrLn stderr "invalid argument\nUSAGE: ./Parse FOLDER" >> exitFailure

parseAndWrite :: String -> IO ()
parseAndWrite f = do
            input <- L.readFile f
            let name = detectEncodingName input
                enc  = encodingFromString (fromMaybe "" name)
                str  = decodeLazyByteString enc input
            case PcbnewExpr.parseWithFilename f str of
                Left err -> hPutStrLn stderr err >> exitFailure
                Right px -> do
                    let outFile = joinPath ["dist/build/parse-tmp/mod-output", f]
                        outDir = takeDirectory outFile
                    createDirectoryIfMissing True outDir
                    L.writeFile outFile
                              $ encodeLazyByteString enc
                              $ show
                              $ PcbnewExpr.pretty px
