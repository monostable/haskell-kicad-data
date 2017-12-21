module Data.Kicad.SExpr.Write
( pretty
, write
)
where
import Data.Char (toLower, isLower, isNumber)
import Data.List (intercalate)
import Text.PrettyPrint.Compact

import Data.Kicad.SExpr.SExpr

{-| Pretty-print a 'SExpr' as a readable 'Doc'. -}
pretty :: SExpr -> Doc
pretty (List _ xs) = text "(" <> align (sep $ map pretty xs) <> text ")"
pretty atm = text $ write atm


{-| Serialize an SExpr as a compact s-expression 'String'. -}
write :: SExpr -> String
write (Atom _ str) |  (str == "")
                       || head str `elem` '.':'-':['0'..'9']
                       || foldr
                           (\c z -> z || c `elem` ')':'(':'\\':'\"':['\0'..' '])
                               False str = show str -- escaped string with quotes
                   | otherwise       = str
write (List _ sxs) = "(" ++ unwords (map write sxs) ++ ")"
