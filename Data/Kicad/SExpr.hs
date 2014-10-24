module Data.Kicad.SExpr
( SExpr(..)
, Keyword(..)
)
where

data SExpr = AtomKey Keyword
           | AtomStr String
           | AtomDbl Double
           | List [SExpr]
    deriving (Eq)

-- shows as a lisp-list hiding all the types
instance Show SExpr where
    show x = squareToBrackets  $ inner x
        where
            inner (AtomKey kw)  = show kw
            inner (AtomStr atm) | (' ' `elem` atm) || (atm == "") = show atm
                                | otherwise = read $ show atm :: String --don't show quotes
            inner (AtomDbl atm) = show atm
            inner (List    atm) = show atm

-- and everything, goes back to the beginning
squareToBrackets :: String -> String
squareToBrackets = map convert
    where convert c = case c of
            '[' -> '('
            ']' -> ')'
            ',' -> ' '
            _   -> c

data Keyword = KeyModule
             | KeyLayer
             | KeyFpText
             | KeyAt
             | KeyEffects
             | KeyFont
             | KeySize
             | KeyThickness
             | KeyTEdit
             | KeyFpLine
             | KeyStart
             | KeyEnd
             | KeyWidth
             | KeyDescr
             | KeyTags
             | KeyAttr
             | KeyPad
             | KeyLayers
             | KeyDrill
             | KeyRectDelta
    deriving (Eq, Enum, Bounded)

instance Show Keyword where
    show x  = case x of
        KeyModule        -> "module"
        KeyLayer         -> "layer"
        KeyFpText        -> "fp_text"
        KeyAt            -> "at"
        KeyEffects       -> "effects"
        KeyFont          -> "font"
        KeySize          -> "size"
        KeyThickness     -> "thickness"
        KeyTEdit         -> "tedit"
        KeyFpLine        -> "fp_line"
        KeyStart         -> "start"
        KeyEnd           -> "end"
        KeyWidth         -> "width"
        KeyDescr         -> "descr"
        KeyTags          -> "tags"
        KeyAttr          -> "attr"
        KeyPad           -> "pad"
        KeyLayers        -> "layers"
        KeyDrill         -> "drill"
        KeyRectDelta     -> "rect_delta"

