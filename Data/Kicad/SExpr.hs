{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Kicad.SExpr
( SExpr(..)
, Keyword(..)
, Writable(..)
)
where
import Data.List (intercalate)
import Data.Char (toLower, isLower)

data SExpr = AtomKey Keyword
           | AtomStr String
           | AtomDbl Double
           | List [SExpr]
    deriving (Show, Eq)

instance Writable SExpr where
    write (AtomKey kw)  = write kw
    write (AtomStr atm) |  (atm == "")
                        || head atm `elem` '.':'-':['0' .. '9']
                        || (foldr (\c z -> z || c `elem` ')':'(':'\\':'\"':['\0' .. ' ']) False atm)
                                       = show atm -- escaped string with quotes
                        | otherwise    = atm
    write (AtomDbl atm) = show atm
    write (List    sxs) = write sxs

instance Writable [SExpr] where
    write sxs = "(" ++ unwords (map write sxs) ++ ")"

{- The keywords must be "Key" ++ a camel-case version of the KiCad ones as the
 - parser and writer use the derived 'Show' instance. The parser will also try
 - them in the order they appear below so KeyAttr has to appear before KeyAt
 - for instance. -}
data Keyword = KeyXyz
             | KeyXy
             | KeyWidth
             | KeyThickness
             | KeyTedit
             | KeyTags
             | KeyStart
             | KeySize
             | KeyScale
             | KeyRotate
             | KeyRectDelta
             | KeyPts
             | KeyPad
             | KeyModule
             | KeyModel
             | KeyLayers
             | KeyLayer
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
             | KeyCenter
             | KeyAttr
             | KeyAt
             | KeyAngle
    deriving (Show, Eq, Enum, Bounded)

instance Writable Keyword where
    -- KeyFpText -> fp_text
    write = intercalate "_" . splitCapWords . drop 3 . show
        where
            splitCapWords "" = []
            splitCapWords (x:xs) =
                let (word, rest) = span isLower xs
                in (toLower x : word) : splitCapWords rest

class Writable a where
    write :: a -> String

instance Writable String where
    write = id
