{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Kicad.SExpr
( SExpr(..)
, Keyword(..)
, Writable(..)
)
where

data SExpr = AtomKey Keyword
           | AtomStr String
           | AtomDbl Double
           | List [SExpr]
    deriving (Show, Eq)

instance Writable SExpr where
    write (AtomKey kw)  = write kw
    write (AtomStr atm) |  (atm == "")
                        || (head atm `elem` '.':'-':['0' .. '9'])
                        || (foldr (\c z -> z || c `elem` ')':'(':'\\':'\"':['\0' .. ' ']) False atm)
                                       = show atm -- escaped string with quotes
                        | otherwise    = atm
    write (AtomDbl atm) = show atm
    write (List    sxs) = write sxs

instance Writable [SExpr] where
    write sxs = "(" ++ unwords (map write sxs) ++ ")"

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
             | KeyAngle
             | KeyFpArc
    deriving (Show, Eq, Enum, Bounded)

instance Writable Keyword where
    write x  = case x of
        KeyModule    -> "module"
        KeyLayer     -> "layer"
        KeyFpText    -> "fp_text"
        KeyAt        -> "at"
        KeyEffects   -> "effects"
        KeyFont      -> "font"
        KeySize      -> "size"
        KeyThickness -> "thickness"
        KeyTEdit     -> "tedit"
        KeyFpLine    -> "fp_line"
        KeyStart     -> "start"
        KeyEnd       -> "end"
        KeyWidth     -> "width"
        KeyDescr     -> "descr"
        KeyTags      -> "tags"
        KeyAttr      -> "attr"
        KeyPad       -> "pad"
        KeyLayers    -> "layers"
        KeyDrill     -> "drill"
        KeyRectDelta -> "rect_delta"
        KeyAngle     -> "angle"
        KeyFpArc     -> "fp_arc"

class Writable a where
    write :: a -> String

instance Writable String where
    write = id
