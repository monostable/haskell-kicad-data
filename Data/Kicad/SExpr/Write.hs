module Data.Kicad.SExpr.Write
( pretty
, write
)
where
import Text.PrettyPrint.Compact

import Data.Kicad.SExpr.SExpr

{-| Pretty-print an s-expression as a readable "document". -}
pretty :: SExpr -> Doc
pretty (List _ xs) = text "(" <> align (sep $ map pretty xs) <> text ")"
pretty atm = text $ write atm


{-| Serialize an s-expression into a compact string. -}
write :: SExpr -> String
write (Atom _ str) | (str == "") || needs_quotes = show str -- escaped string with quotes
                   | otherwise                   = str      -- bare string without quotes
    where needs_quotes = foldr (\c z -> z || c `elem` ')':'(':'\\':'\"':['\0'..' ']) False str
write (List _ sxs) = "(" ++ unwords (map write sxs) ++ ")"
