module Data.Kicad.SExpr.SExpr
( SExpr(..)
, SExpressable(..)
, getPos
)
where
import Text.Parsec (SourcePos)

data SExpr = Atom SourcePos String
           | List SourcePos [SExpr]
    deriving (Show, Eq)

class SExpressable a where
    toSExpr :: a -> SExpr

{-| Get s-expression source code position (filename, line-number and
 - column-number) -}
getPos :: SExpr -> SourcePos
getPos (Atom pos _) = pos
getPos (List pos _) = pos
