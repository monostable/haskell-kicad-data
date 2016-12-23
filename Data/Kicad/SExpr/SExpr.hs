module Data.Kicad.SExpr.SExpr
( SExpr(..)
, Keyword(..)
, SExpressable(..)
)
where

data SExpr = AtomKey Keyword
           | AtomStr String
           | AtomDbl Double
           | List [SExpr]
    deriving (Show, Eq)

{- The keywords _must_ be "Key" ++ camel-case version of the ones in the
 - kicad_mod files as the parser and writer use the derived 'Show' instance.
 - The parser will also try them in the order they appear below so KeyAttr has
 - to appear before KeyAt for instance. -}
data Keyword = KeyZoneConnect
             | KeyXyz
             | KeyXy
             | KeyWidth
             | KeyThickness
             | KeyThermalWidth
             | KeyThermalGap
             | KeyTedit
             | KeyTags
             | KeyStart
             | KeySolderPasteMarginRatio
             | KeySolderPasteMargin
             | KeySolderMaskMargin
             | KeySize
             | KeyScale
             | KeyRoundrectRratio
             | KeyRotate
             | KeyRectDelta
             | KeyPts
             | KeyPath
             | KeyPad
             | KeyOffset
             | KeyModule
             | KeyModel
             | KeyLayers
             | KeyLayer
             | KeyJustify
             | KeyFpText
             | KeyFpPoly
             | KeyFpLine
             | KeyFpCircle
             | KeyFpArc
             | KeyFont
             | KeyEnd
             | KeyEffects
             | KeyDrill
             | KeyDescr
             | KeyClearance
             | KeySolderPasteRatio
             | KeyCenter
             | KeyAutoplaceCost90
             | KeyAutoplaceCost180
             | KeyAttr
             | KeyAt
             | KeyAngle
    deriving (Show, Eq, Enum, Bounded)

class SExpressable a where
    toSExpr :: a -> SExpr
