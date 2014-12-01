module Data.Kicad.SExpr.Write
( pretty
, write
, writeKeyword
)
where
import Data.Char (toLower, isLower, isNumber)
import Data.List (intercalate)
import Text.PrettyPrint.Compact

import Data.Kicad.SExpr.SExpr

pretty :: SExpr -> Doc
pretty (List xs) = text "(" <> align (sep $ map pretty xs) <> text ")"
pretty atm = text $ write atm

write :: SExpr -> String
write (AtomKey kw)  = writeKeyword kw
write (AtomStr atm) |  (atm == "")
                    || head atm `elem` '.':'-':['0'..'9']
                    || foldr
                        (\c z -> z || c `elem` ')':'(':'\\':'\"':['\0'..' '])
                            False atm = show atm -- escaped string with quotes
                    | otherwise       = atm
-- this should just be printf "%g" but that doesn't work as it should
write (AtomDbl atm) = strip_zeros $ break (== '.') $ show atm
    where strip_zeros (s1,s2) = s1 ++ dot_if_needed (reverse
                                           $ dropWhile (=='0') $ reverse s2)
          dot_if_needed s     = if s == "." then "" else s
write (List    sxs) = "(" ++ unwords (map write sxs) ++ ")"

writeKeyword :: Keyword -> String
writeKeyword = intercalate "_" . splitCapWords . drop 3 . show
    where
        splitCapWords "" = []
        splitCapWords (x:xs) =
            let (word, rest) = span (\c -> isLower c || isNumber c) xs
            in (toLower x : word) : splitCapWords rest

