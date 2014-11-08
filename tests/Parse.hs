import System.Environment
import System.Exit
import Data.Kicad.Parse
import Data.Kicad.KicadExpr
import Text.PrettyPrint.Leijen


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "invalid argument\nUSAGE: ./Parse [FILES]"
        fs -> parseAndDisplay fs

parseAndDisplay :: [String] -> IO ()
parseAndDisplay [] = return ()
parseAndDisplay (f:fs) = do
            input <- readFile f
            case parse input of
                Left err -> putStrLn f >> putStrLn err >> exitFailure
                Right kx -> putDoc (pretty (toSExpr kx)) >> parseAndDisplay fs

