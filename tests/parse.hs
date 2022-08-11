import Codec.Text.Detect (detectEncodingName)
import Control.Concurrent.Async (mapConcurrently_)
import Data.Encoding
import Data.Encoding.UTF8
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
        [ext, input_folder, output_folder] -> do
            pw <- pathWalkLazy input_folder
            let files = pw >>= (\(root, dirs, files) ->
                        files >>= (\file ->
                            if (("." ++ ext) `isSuffixOf` file) then [joinPath [root, file]] else []))
            putStrLn $ "Running parse on " ++ (show (length files)) ++ " ." ++ ext ++ " files"
            _ <- mapConcurrently_ (parseAndWrite ext output_folder) files
            return ()
        _ -> hPutStrLn stderr "invalid arguments\nUSAGE: ./parse <FILE_EXTENSION> <INPUT_FOLDER> <OUTPUT_FOLDER>" >> exitFailure

parseAndWrite :: String -> String -> String -> IO ()
parseAndWrite ext output_folder f = do
            input <- L.readFile f
            let name = detectEncodingName input
                enc  = fromMaybe (encodingFromString "utf8") (name >>= encodingFromStringExplicit)
                str  = decodeLazyByteString enc input
                parse = case ext of
                          "kicad_mod" -> PcbnewExpr.parseWithFilename
                          _           -> error "unimplemented"
            case parse f str of
                Left err -> hPutStrLn stderr err >> exitFailure
                Right px -> do
                    let outFile = joinPath [output_folder, f]
                        outDir = takeDirectory outFile
                    createDirectoryIfMissing True outDir
                    L.writeFile outFile
                              $ encodeLazyByteString enc
                              $ show
                              $ PcbnewExpr.pretty px
