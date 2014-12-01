module Data.Kicad.PcbnewExpr.Write
(
  pretty
, write
)
where
import Text.PrettyPrint.Compact

import Data.Kicad.PcbnewExpr.PcbnewExpr
import qualified Data.Kicad.SExpr as SExpr

{-| Pretty-print a 'PcbnewExpr' as a readable s-expression 'Doc'.-}
pretty :: PcbnewExpr -> Doc
pretty = SExpr.pretty . SExpr.toSExpr

{-| Serialize a 'PcbnewExpr' as a compact s-expression 'String'. -}
write :: PcbnewExpr -> String
write = SExpr.write . SExpr.toSExpr

