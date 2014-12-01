module Data.Kicad.PcbnewExpr.Write
( write
, pretty
)
where
import Text.PrettyPrint.Compact

import Data.Kicad.PcbnewExpr.PcbnewExpr
import qualified Data.Kicad.SExpr as SExpr

pretty :: PcbnewExpr -> Doc
pretty = SExpr.pretty . SExpr.toSExpr

write :: PcbnewExpr -> String
write = SExpr.write . SExpr.toSExpr
