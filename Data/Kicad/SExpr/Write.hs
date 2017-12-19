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
pretty (List xs) = text "(" <> align (sep $ map pretty xs) <> text ")"
pretty atm = text $ write atm


{-| Serialize an SExpr as a compact s-expression 'String'. -}
write :: SExpr -> String
write (Atom atm) |  (atm == "")
                     || head atm `elem` '.':'-':['0'..'9']
                     || foldr
                         (\c z -> z || c `elem` ')':'(':'\\':'\"':['\0'..' '])
                             False atm = show atm -- escaped string with quotes
                 | otherwise       = atm
write (List    sxs) = "(" ++ unwords (map write sxs) ++ ")"
