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
data Keyword =
               KeyAngle
             | KeyAttr
             | KeyAt
             | KeyAutoplaceCost180
             | KeyAutoplaceCost90
             | KeyCenter
             | KeyClearance
             | KeyDescr
             | KeyDrill
             | KeyEffects
             | KeyEnd
             | KeyFont
             | KeyFpArc
             | KeyFpCircle
             | KeyFpLine
             | KeyFpPoly
             | KeyFpText
             | KeyJustify
             | KeyLayers
             | KeyLayer
             | KeyModel
             | KeyModule
             | KeyOffset
             | KeyPad
             | KeyPath
             | KeyPts
             | KeyRectDelta
             | KeyRotate
             | KeyRoundrectRratio
             | KeyScale
             | KeySize
             | KeySolderMaskMargin
             | KeySolderPasteMarginRatio
             | KeySolderPasteMargin
             | KeySolderPasteRatio
             | KeyStart
             | KeyTags
             | KeyTedit
             | KeyThermalGap
             | KeyThermalWidth
             | KeyThickness
             | KeyWidth
             | KeyXyz
             | KeyXy
             | KeyZoneConnect
    deriving (Show, Eq, Enum, Bounded)

class SExpressable a where
    toSExpr :: a -> SExpr
