module Data.Kicad.Parse
( parse
)
where
import Data.Kicad.Internal.ParseSExpr
import Data.Kicad.PcbnewExpr
import Data.Kicad.Internal.Interpret

parse :: String -> Either String PcbnewExpr
parse = either Left interpret . parseSExpr
