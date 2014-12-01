import System.Environment
import System.Exit
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
            input <- readFile f
            case PcbnewExpr.parse input of
                Left err -> putStrLn f >> putStrLn err >> exitFailure
                Right px -> print (PcbnewExpr.pretty px) >> parseAndDisplay fs

