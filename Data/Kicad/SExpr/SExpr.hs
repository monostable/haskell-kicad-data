module Data.Kicad.SExpr.SExpr
( SExpr(..)
, SExpressable(..)
)
where

data SExpr = Atom String
           | List [SExpr]
    deriving (Show, Eq)

class SExpressable a where
    toSExpr :: a -> SExpr
