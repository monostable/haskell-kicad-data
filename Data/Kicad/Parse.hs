module Data.Kicad.Parse
( parse
)
where
import Data.Kicad.ParseSExpr
import Data.Kicad.KicadExpr
import Data.Kicad.Interpret

parse :: String -> Either String KicadExpr
parse = either Left interpret . parseSExpr
