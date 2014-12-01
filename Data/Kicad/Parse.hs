module Data.Kicad.Parse
( parse
)
where
import Data.Kicad.ParseSExpr
import Data.Kicad.PcbnewExpr
import Data.Kicad.Interpret

parse :: String -> Either String PcbnewExpr
parse = either Left interpret . parseSExpr
