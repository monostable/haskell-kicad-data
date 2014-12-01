module Data.Kicad.PcbnewExpr.Write
( write
, pretty
)
where
import Text.PrettyPrint.Compact

import Data.Kicad.PcbnewExpr.PcbnewExpr
import qualified Data.Kicad.SExpr as SExpr

{-| Serialize a PcbnewExpr as a compact s-expression string -}
write :: PcbnewExpr -> String
write = SExpr.write . SExpr.toSExpr

{-| Pretty-print a PcbnewExpr as an indented s-expression-}
pretty :: PcbnewExpr -> Doc
pretty = SExpr.pretty . SExpr.toSExpr
