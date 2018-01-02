{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Kicad.PcbnewExpr.PcbnewExpr
(
-- * Types
  PcbnewExpr(..)
, PcbnewModule(..)
, PcbnewKicadPcb(..)
, PcbnewItem(..)
, PcbnewAttribute(..)
-- * Attribute types
, PcbnewDrillT(..)
, PcbnewAtT(..)
, PcbnewLayerT(..)
, PcbnewPadShapeT(..)
, PcbnewPadTypeT(..)
, PcbnewFpTextTypeT(..)
, PcbnewJustifyT(..)
, PcbnewXyzT
, V2Double
-- * Lenses and other getters/setters
, fpTextJustify
, moduleItems
, moduleAttrs
, kicadPcbAttrs
, itemLayers
, padAttributes
, atP
, atX
, atY
, itemsOn
, itemPoints
, itemHandle
-- * String conversion
, strToLayer
, layerToStr
, strToPadType
, fpPadTypeToStr
, strToPadShape
, fpPadShapeToStr
, strToFpTextType
, fpTextTypeToStr
, strToJustify
, justifyToString
-- * Default (empty) instances
, defaultPcbnewModule
, defaultPcbnewKicadPcb
, defaultPcbnewFpText
, defaultPcbnewFpLine
, defaultPcbnewFpCircle
, defaultPcbnewFpArc
, defaultPcbnewFpPoly
, defaultPcbnewPad
, defaultPcbnewDrillT
, defaultPcbnewFont
, defaultPcbnewModel
, defaultPcbnewAtT
)
where
import Lens.Family2
import Data.AEq
import Data.Tuple (swap)
import Data.Maybe
import Data.Foldable (foldMap)
import Text.Parsec.Pos (newPos, SourcePos)

import Data.Kicad.SExpr.SExpr
import Data.Kicad.Util

data PcbnewExpr = PcbnewExprModule PcbnewModule
                | PcbnewExprKicadPcb PcbnewKicadPcb
                | PcbnewExprItem PcbnewItem
                | PcbnewExprAttribute PcbnewAttribute
    deriving (Show, Eq)

instance AEq PcbnewExpr where
    PcbnewExprModule    x ~== PcbnewExprModule    y = x ~== y
    PcbnewExprKicadPcb  x ~== PcbnewExprKicadPcb  y = x ~== y
    PcbnewExprItem      x ~== PcbnewExprItem      y = x ~== y
    PcbnewExprAttribute x ~== PcbnewExprAttribute y = x ~== y
    _ ~== _ = False

instance SExpressable PcbnewExpr where
    toSExpr (PcbnewExprModule x)    = toSExpr x
    toSExpr (PcbnewExprItem x)      = toSExpr x
    toSExpr (PcbnewExprAttribute x) = toSExpr x

data PcbnewModule = PcbnewModule { pcbnewModuleName  :: String
                                 , pcbnewModuleLayer :: PcbnewLayerT
                                 , pcbnewModuleAttrs :: [PcbnewAttribute]
                                 , pcbnewModuleItems :: [PcbnewItem]
                                 }
    deriving (Show, Eq)

data PcbnewKicadPcb = PcbnewKicadPcb
    { pcbnewKicadPcbAttrs   :: [PcbnewAttribute]
    , pcbnewKicadPcbNets    :: [PcbnewItem]
    , pcbnewKicadPcbModules :: [PcbnewModule]
    , pcbnewKicadPcbItems   :: [PcbnewItem]
    }
        deriving (Show, Eq)

defaultPcbnewKicadPcb :: PcbnewKicadPcb
defaultPcbnewKicadPcb = PcbnewKicadPcb
    { pcbnewKicadPcbAttrs   = []
    , pcbnewKicadPcbNets    = []
    , pcbnewKicadPcbModules = []
    , pcbnewKicadPcbItems   = []
    }

instance AEq PcbnewKicadPcb where
    PcbnewKicadPcb as1 ns1 ms1 is1 ~== PcbnewKicadPcb as2 ns2 ms2 is2 =
          as1 ~== as2
       && ns1 ~== ns2
       && ms1 ~== ms2
       && is1 ~== is2


pos :: SourcePos
pos = newPos "" 0 0

instance SExpressable PcbnewModule where
    toSExpr (PcbnewModule name l attrs items) =
        List pos $ [ Atom pos "module"
               , Atom pos name
               , toSExpr (PcbnewLayer l)
               ] ++ map toSExpr attrs
               ++ map toSExpr items

defaultPcbnewModule :: PcbnewModule
defaultPcbnewModule = PcbnewModule "" FCu [] []

moduleItems :: Functor f => LensLike' f PcbnewModule [PcbnewItem]
moduleItems f (PcbnewModule n l a i) = PcbnewModule n l a `fmap` f i

moduleAttrs :: Functor f => LensLike' f PcbnewModule [PcbnewAttribute]
moduleAttrs f (PcbnewModule n l a i) = (\a' -> PcbnewModule n l a' i) `fmap` f a

kicadPcbAttrs :: Functor f => LensLike' f PcbnewKicadPcb [PcbnewAttribute]
kicadPcbAttrs f (PcbnewKicadPcb a n m i) = (\a' -> PcbnewKicadPcb a' n m i) `fmap` f a

instance AEq PcbnewModule where
    PcbnewModule n1 l1 as1 is1 ~== PcbnewModule n2 l2 as2 is2 =
           n1   == n2
        && l1   == l2
        && as1 ~== as2
        && is1 ~== is2

data PcbnewItem = PcbnewFpText { fpTextType      :: PcbnewFpTextTypeT
                               , fpTextStr       :: String
                               , itemAt          :: PcbnewAtT
                               , itemLayer       :: PcbnewLayerT
                               , fpTextHide      :: Bool
                               , itemSize        :: V2Double
                               , fpTextThickness :: Double
                               , fpTextItalic    :: Bool
                               , fpTextJustify_  :: [PcbnewJustifyT]
                               }
                | PcbnewFpLine { itemStart :: V2Double
                               , itemEnd   :: V2Double
                               , itemLayer :: PcbnewLayerT
                               , itemWidth :: Double
                               }
                | PcbnewFpCircle { itemStart  :: V2Double
                                 , itemEnd    :: V2Double
                                 , itemLayer  :: PcbnewLayerT
                                 , itemWidth  :: Double
                                 }
                | PcbnewFpArc { itemStart  :: V2Double
                              , itemEnd    :: V2Double
                              , fpArcAngle :: Double
                              , itemLayer  :: PcbnewLayerT
                              , itemWidth  :: Double
                              }
                | PcbnewFpPoly { fpPolyPts :: [V2Double]
                               , itemLayer :: PcbnewLayerT
                               , itemWidth :: Double
                               }
                | PcbnewPad { padNumber      :: String
                            , padType        :: PcbnewPadTypeT
                            , padShape       :: PcbnewPadShapeT
                            , itemAt         :: PcbnewAtT
                            , itemSize       :: V2Double
                            , padLayers      :: [PcbnewLayerT]
                            , padAttributes_ :: [PcbnewAttribute]
                            }
    deriving (Show, Eq)


{-| Lense of the points that define this item -}
itemPoints :: Functor f => LensLike' f PcbnewItem [V2Double]
itemPoints f item = case item of
    PcbnewFpText {}   -> atLense
    PcbnewPad {}      -> atLense
    PcbnewFpLine {}   -> startEndLense
    PcbnewFpCircle {} -> startEndLense
    PcbnewFpArc {}    -> startEndLense
    PcbnewFpPoly {}   -> polyLense
    where
        atLense     = atSetter `fmap` (f [view atP (itemAt item)])
        atSetter ps =
            fromMaybe
                item
                (fmap (\p -> item {itemAt = set atP p (itemAt item)})
                    (maybeHead ps))
        startEndLense =
            startEndSetter `fmap` (f [itemStart item, itemEnd item])
        startEndSetter (p1:p2:_) = item {itemStart = p1, itemEnd = p2}
        startEndSetter (p1:[])   = item {itemStart = p1}
        startEndSetter _         = item
        polyLense = (\ps -> item {fpPolyPts = ps}) `fmap` (f (fpPolyPts item))


{-| Lense of the item handle, moving the handle will move the entire item -}
itemHandle :: Functor f => LensLike' f PcbnewItem V2Double
itemHandle f item = setter `fmap` (f (headOr (0,0) (view itemPoints item)))
    where
        setter p = let diff = (view itemHandle item) - p
                   in over itemPoints (map (+ diff)) item


instance SExpressable PcbnewItem where
    toSExpr (PcbnewFpText t s a l h si th i j) =
        List pos $ [ Atom pos "fp_text"
               , Atom pos $ fpTextTypeToStr t
               , Atom pos s
               , toSExpr (PcbnewAt a)
               , toSExpr (PcbnewLayer l)
               ]
               ++ [Atom pos "hide" | h]
               ++ [toSExpr $ PcbnewFpTextEffects $
                      [PcbnewFont si th i]
                      ++ if j == [] then [] else [PcbnewJustify j]]
    toSExpr (PcbnewFpLine s e l w) =
        List pos [ Atom pos "fp_line"
             , toSExpr (PcbnewStart s)
             , toSExpr (PcbnewEnd   e)
             , toSExpr (PcbnewLayer l)
             , toSExpr (PcbnewWidth w)
             ]
    toSExpr (PcbnewFpCircle s e l w) =
        List pos [ Atom pos "fp_circle"
             , toSExpr (PcbnewCenter s)
             , toSExpr (PcbnewEnd    e)
             , toSExpr (PcbnewLayer  l)
             , toSExpr (PcbnewWidth  w)
             ]
    toSExpr (PcbnewFpArc s e a l w) =
        List pos [ Atom pos "fp_arc"
             , toSExpr (PcbnewStart s)
             , toSExpr (PcbnewEnd   e)
             , toSExpr (PcbnewAngle a)
             , toSExpr (PcbnewLayer l)
             , toSExpr (PcbnewWidth w)
             ]
    toSExpr (PcbnewFpPoly ps l w) =
        List pos [ Atom pos "fp_poly"
             , toSExpr (PcbnewPts ps)
             , toSExpr (PcbnewLayer l)
             , toSExpr (PcbnewWidth w)
             ]
    toSExpr (PcbnewPad n t s a si l attrs) =
        List pos $ [ Atom pos "pad"
               , Atom pos n
               , Atom pos $ fpPadTypeToStr t
               , Atom pos $ fpPadShapeToStr s
               , toSExpr $ PcbnewAt a
               , toSExpr $ PcbnewSize si
               , toSExpr $ PcbnewLayers l
               ] ++ map toSExpr attrs

itemLayers :: Functor f => LensLike' f PcbnewItem [PcbnewLayerT]
itemLayers f item@(PcbnewPad { }) =
    (\ls -> item {padLayers = ls}) `fmap` f (padLayers item)
itemLayers f item = update `fmap` f [itemLayer item]
    where update [] = item
          update ls = item {itemLayer = head ls}

padAttributes :: Functor f => LensLike' f PcbnewItem [PcbnewAttribute]
padAttributes f i = (\as -> i {padAttributes_ = as}) `fmap` f (padAttributes_ i)

instance AEq PcbnewItem where
    (PcbnewFpText t1 s1 a1 l1 h1 si1 th1 i1 j1)
        ~== (PcbnewFpText t2 s2 a2 l2 h2 si2 th2 i2 j2) =
           t1   == t2
        && s1   == s2
        && a1  ~== a2
        && l1   == l2
        && h1   == h2
        && si1 ~== si2
        && th1 ~== th2
        && i1   == i2
        && j1   == j2
    (PcbnewFpLine s1 e1 l1 w1) ~== (PcbnewFpLine s2 e2 l2 w2) =
           s1 ~== s2
        && e1 ~== e2
        && l1  == l2
        && w1 ~== w2
    (PcbnewFpCircle s1 e1 l1 w1) ~== (PcbnewFpCircle s2 e2 l2 w2) =
           s1 ~== s2
        && e1 ~== e2
        && l1  == l2
        && w1 ~== w2
    (PcbnewFpArc s1 e1 a1 l1 w1) ~== (PcbnewFpArc s2 e2 a2 l2 w2) =
           s1 ~== s2
        && e1 ~== e2
        && a1 ~== a2
        && l1  == l2
        && w1 ~== w2
    (PcbnewFpPoly ps1 l1 w1) ~== (PcbnewFpPoly ps2 l2 w2) =
           ps1 ~== ps2
        && l1   == l2
        && w1  ~== w2
    (PcbnewPad n1 t1 s1 a1 si1 l1 attrs1)
        ~== (PcbnewPad n2 t2 s2 a2 si2 l2 attrs2) =
           n1   == n2
        && t1   == t2
        && s1   == s2
        && a1  ~== a2
        && si1 ~== si2
        && l1   == l2
        && attrs1 ~== attrs2
    x ~== y = x == y

defaultPcbnewFpText :: PcbnewItem
defaultPcbnewFpText = PcbnewFpText { fpTextType      = FpTextUser
                                   , fpTextStr       = ""
                                   , itemAt          = defaultPcbnewAtT
                                   , itemLayer       = FSilkS
                                   , fpTextHide      = False
                                   , itemSize        = (1.0, 1.0)
                                   , fpTextThickness = 1.0
                                   , fpTextItalic    = False
                                   , fpTextJustify_  = []
                                   }

defaultPcbnewFpLine :: PcbnewItem
defaultPcbnewFpLine = PcbnewFpLine { itemStart = (0,0)
                                   , itemEnd   = (0,0)
                                   , itemLayer = FSilkS
                                   , itemWidth = 0.15
                                   }

defaultPcbnewFpCircle :: PcbnewItem
defaultPcbnewFpCircle = PcbnewFpCircle { itemStart = (0,0)
                                       , itemEnd   = (0,0)
                                       , itemLayer = FSilkS
                                       , itemWidth = 0.15
                                       }
defaultPcbnewFpArc :: PcbnewItem
defaultPcbnewFpArc = PcbnewFpArc { itemStart  = (0,0)
                                 , itemEnd    = (0,0)
                                 , fpArcAngle = 0
                                 , itemLayer  = FSilkS
                                 , itemWidth = 0.15
                                 }

defaultPcbnewFpPoly :: PcbnewItem
defaultPcbnewFpPoly = PcbnewFpPoly { fpPolyPts   = []
                                   , itemLayer   = FSilkS
                                   , itemWidth = 0.15
                                   }

defaultPcbnewPad :: PcbnewItem
defaultPcbnewPad = PcbnewPad { padNumber      = ""
                             , padType        = ThruHole
                             , padShape       = Circle
                             , itemAt         = defaultPcbnewAtT
                             , itemSize       = (0,0)
                             , padLayers      = []
                             , padAttributes_ = []
                             }

data PcbnewDrillT = PcbnewDrillT { pcbnewDrillSize   :: Maybe V2Double
                                 , pcbnewDrillOval   :: Bool
                                 , pcbnewDrillOffset :: Maybe V2Double
                                 }
    deriving (Show, Eq)

defaultPcbnewDrillT :: PcbnewDrillT
defaultPcbnewDrillT  = PcbnewDrillT Nothing False Nothing

instance AEq PcbnewDrillT where
    PcbnewDrillT s1 o1 off1 ~== PcbnewDrillT s2 o2 off2
        = s1 ~== s2 && o1 == o2 && off1 ~== off2

data PcbnewAttribute = PcbnewLayer      PcbnewLayerT
                     | PcbnewAt         PcbnewAtT
                     | PcbnewFpTextType PcbnewFpTextTypeT
                     | PcbnewSize       V2Double
                     | PcbnewThickness  Double
                     | PcbnewTedit      String
                     | PcbnewItalic
                     | PcbnewHide
                     | PcbnewPlaced
                     | PcbnewLocked
                     | PcbnewStart      V2Double
                     | PcbnewCenter     V2Double
                     | PcbnewEnd        V2Double
                     | PcbnewWidth      Double
                     | PcbnewDescr      String
                     | PcbnewTags       String
                     | PcbnewPath       String
                     | PcbnewAttr       String
                     | PcbnewLayers     [PcbnewLayerT]
                     | PcbnewDrill      PcbnewDrillT
                     | PcbnewRectDelta  V2Double
                     | PcbnewFpTextEffects [PcbnewAttribute]
                     | PcbnewFont { pcbnewFontSize :: V2Double
                                  , pcbnewFontThickness :: Double
                                  , pcbnewFontItalic :: Bool
                                  }
                     | PcbnewAngle Double
                     | PcbnewXy    V2Double
                     | PcbnewPts   [V2Double]
                     | PcbnewModel { pcbnewModelPath   :: String
                                   , pcbnewModelAt     :: PcbnewXyzT
                                   , pcbnewModelScale  :: PcbnewXyzT
                                   , pcbnewModelRotate :: PcbnewXyzT
                                   }
                     | PcbnewModelAt           PcbnewAttribute
                     | PcbnewModelScale        PcbnewAttribute
                     | PcbnewModelRotate       PcbnewAttribute
                     | PcbnewXyz               PcbnewXyzT
                     | PcbnewClearance         Double
                     | PcbnewSolderPasteRatio  Double
                     | PcbnewMaskMargin        Double
                     | PcbnewPasteMargin       Double
                     | PcbnewPasteMarginRatio  Double
                     | PcbnewRoundrectRratio   Double
                     | PcbnewOffset            V2Double
                     | PcbnewAutoplaceCost90   Int
                     | PcbnewAutoplaceCost180  Int
                     | PcbnewZoneConnect       Int
                     | PcbnewThermalWidth      Double
                     | PcbnewThermalGap        Double
                     | PcbnewJustify           [PcbnewJustifyT]
                     | PcbnewDieLength         Double
    deriving (Show, Eq)



type PcbnewXyzT = (Double, Double, Double)

instance SExpressable PcbnewAttribute where
    toSExpr (PcbnewLayer l) = List pos [ Atom pos "layer"
                                   , Atom pos $ layerToStr l
                                   ]
    toSExpr (PcbnewAt (PcbnewAtT (x,y) o)) =
        List pos $ [ Atom pos "at"
               , atomDbl x
               , atomDbl y
               ] ++ [atomDbl o | o /= 0]
    toSExpr (PcbnewLayers ls) =
        List pos (Atom pos "layers" : map (Atom pos . layerToStr) ls)
    toSExpr (PcbnewFont s t i) =
        List pos $ [ Atom pos "font", toSExpr (PcbnewSize s)
               , toSExpr (PcbnewThickness t)
               ] ++ [Atom pos "italic" | i]
    toSExpr (PcbnewPts xys) =
        List pos $ Atom pos "pts" : map (toSExpr . PcbnewXy) xys
    toSExpr (PcbnewModel p a s r) =
        List pos [ Atom pos "model"
             , Atom pos p
             , toSExpr (PcbnewModelAt     (PcbnewXyz a))
             , toSExpr (PcbnewModelScale  (PcbnewXyz s))
             , toSExpr (PcbnewModelRotate (PcbnewXyz r))
             ]
    toSExpr (PcbnewDrill (PcbnewDrillT s o off)) =
        List pos $ [Atom pos "drill"]
             ++ [Atom pos "oval" | o]
             ++ (if o && isJust s
                then [atomDbl (fst (fromJust s)), atomDbl (snd (fromJust s))]
                else [atomDbl (fst (fromJust s)) | isJust s])
             ++ [toSExpr (PcbnewOffset (fromJust off)) | isJust off]
    toSExpr (PcbnewXyz (x,y,z)) =
        List pos [Atom pos "xyz", atomDbl x, atomDbl y, atomDbl z]
    toSExpr (PcbnewFpTextEffects l)  = List pos $ [Atom pos "effects"] ++ fmap toSExpr l
    toSExpr (PcbnewFpTextType t)     = Atom pos $ fpTextTypeToStr t
    toSExpr (PcbnewModelAt     xyz)  = List pos [Atom pos "at"    , toSExpr xyz]
    toSExpr (PcbnewModelScale  xyz)  = List pos [Atom pos "scale" , toSExpr xyz]
    toSExpr (PcbnewModelRotate xyz)  = List pos [Atom pos "rotate", toSExpr xyz]
    toSExpr (PcbnewClearance   d)      = toSxD "clearance"                 d
    toSExpr (PcbnewSolderPasteRatio d) = toSxD "solder_paste_ratio"        d
    toSExpr (PcbnewMaskMargin  d)      = toSxD "solder_mask_margin"        d
    toSExpr (PcbnewPasteMargin d)      = toSxD "solder_paste_margin"       d
    toSExpr (PcbnewPasteMarginRatio d) = toSxD "solder_paste_margin_ratio" d
    toSExpr (PcbnewRoundrectRratio  d) = toSxD "roundrect_rratio"          d
    toSExpr (PcbnewThickness   d)      = toSxD "thickness"                 d
    toSExpr (PcbnewWidth       d)      = toSxD "width"                     d
    toSExpr (PcbnewAngle       d)      = toSxD "angle"                     d
    toSExpr (PcbnewThermalWidth d)     = toSxD "thermal_width"             d
    toSExpr (PcbnewThermalGap   d)     = toSxD "thermal_gap"               d
    toSExpr (PcbnewDieLength   d)      = toSxD "die_length"                d
    toSExpr (PcbnewSize      xy)       = toSxDD "size"       xy
    toSExpr (PcbnewStart     xy)       = toSxDD "start"      xy
    toSExpr (PcbnewCenter    xy)       = toSxDD "center"     xy
    toSExpr (PcbnewRectDelta xy)       = toSxDD "rect_delta" xy
    toSExpr (PcbnewEnd       xy)       = toSxDD "end"        xy
    toSExpr (PcbnewXy        xy)       = toSxDD "xy"         xy
    toSExpr (PcbnewOffset    xy)       = toSxDD "offset"     xy
    toSExpr (PcbnewTedit s)            = toSxStr "tedit" s
    toSExpr (PcbnewDescr s)            = toSxStr "descr" s
    toSExpr (PcbnewTags  s)            = toSxStr "tags"  s
    toSExpr (PcbnewPath  s)            = toSxStr "path"  s
    toSExpr (PcbnewAttr  s)            = toSxStr "attr"  s
    toSExpr PcbnewItalic               = Atom pos "italic"
    toSExpr PcbnewHide                 = Atom pos "hide"
    toSExpr PcbnewPlaced               = Atom pos "placed"
    toSExpr PcbnewLocked               = Atom pos "locked"
    toSExpr (PcbnewAutoplaceCost90  i) =
        List pos [Atom pos "autoplace_cost90"  , Atom pos (show i)]
    toSExpr (PcbnewAutoplaceCost180 i) =
        List pos [Atom pos "autoplace_cost180" , Atom pos (show i)]
    toSExpr (PcbnewZoneConnect      i) =
        List pos [Atom pos "zone_connect"      , Atom pos (show i)]
    toSExpr (PcbnewJustify         js) =
        List pos $ (Atom pos "justify"):map (Atom pos . justifyToString) js


atomDbl :: Double -> SExpr
atomDbl = Atom pos . show

toSxD :: String -> Double -> SExpr
toSxD kw d = List pos [Atom pos kw, atomDbl d]

toSxDD :: String -> V2Double -> SExpr
toSxDD kw (x,y) = List pos [Atom pos kw, atomDbl x, atomDbl y]

toSxStr :: String -> String -> SExpr
toSxStr kw s = List pos [Atom pos kw, Atom pos s]

instance AEq PcbnewAttribute where
    (PcbnewAt                x) ~== (PcbnewAt                y) = x ~== y
    (PcbnewSize              x) ~== (PcbnewSize              y) = x ~== y
    (PcbnewCenter            x) ~== (PcbnewCenter            y) = x ~== y
    (PcbnewThickness         x) ~== (PcbnewThickness         y) = x ~== y
    (PcbnewStart             x) ~== (PcbnewStart             y) = x ~== y
    (PcbnewEnd               x) ~== (PcbnewEnd               y) = x ~== y
    (PcbnewWidth             x) ~== (PcbnewWidth             y) = x ~== y
    (PcbnewDrill             x) ~== (PcbnewDrill             y) = x ~== y
    (PcbnewRectDelta         x) ~== (PcbnewRectDelta         y) = x ~== y
    (PcbnewAngle             x) ~== (PcbnewAngle             y) = x ~== y
    (PcbnewXy                x) ~== (PcbnewXy                y) = x ~== y
    (PcbnewPts               x) ~== (PcbnewPts               y) = x ~== y
    (PcbnewXyz               x) ~== (PcbnewXyz               y) = x ~== y
    (PcbnewOffset            x) ~== (PcbnewOffset            y) = x ~== y
    (PcbnewClearance         x) ~== (PcbnewClearance         y) = x ~== y
    (PcbnewMaskMargin        x) ~== (PcbnewMaskMargin        y) = x ~== y
    (PcbnewPasteMargin       x) ~== (PcbnewPasteMargin       y) = x ~== y
    (PcbnewPasteMarginRatio  x) ~== (PcbnewPasteMarginRatio  y) = x ~== y
    (PcbnewThermalWidth      x) ~== (PcbnewThermalWidth      y) = x ~== y
    (PcbnewThermalGap        x) ~== (PcbnewThermalGap        y) = x ~== y
    (PcbnewModelAt           x) ~== (PcbnewModelAt           y) = x ~== y
    (PcbnewModelScale        x) ~== (PcbnewModelScale        y) = x ~== y
    (PcbnewModelRotate       x) ~== (PcbnewModelRotate       y) = x ~== y
    (PcbnewModel p1 a1 s1 r1)   ~== (PcbnewModel p2 a2 s2 r2) =
        p1 == p2 && a1 ~== a2 && s1 ~== s2 && r1 ~== r2
    (PcbnewFont s1 t1 i1) ~== (PcbnewFont s2 t2 i2) =
        s1 ~== s2 && t1 ~== t2 && i1 == i2
    x ~== y = x == y

defaultPcbnewFont :: PcbnewAttribute
defaultPcbnewFont = PcbnewFont { pcbnewFontSize = (1.0, 1.0)
                               , pcbnewFontThickness = 1.0
                               , pcbnewFontItalic = False
                               }

defaultPcbnewModel :: PcbnewAttribute
defaultPcbnewModel = PcbnewModel { pcbnewModelPath   = ""
                                 , pcbnewModelAt     = (0,0,0)
                                 , pcbnewModelScale  = (0,0,0)
                                 , pcbnewModelRotate = (0,0,0)
                                 }

data PcbnewLayerT = FSilkS    | FCu       | FPaste    | FMask     | BSilkS
                  | BCu       | BPaste    | BMask     | DwgsUser  | CmtsUser
                  | FAdhes    | AllSilk   | FandBCu   | AllCu     | AllMask
                  | AllPaste  | EdgeCuts  | FCrtYd    | BCrtYd    | FFab
                  | BFab      | AllFab    | Margin    | Eco1User  | Eco2User
                  | BAdhes
                  | Inner1Cu  | Inner2Cu  | Inner3Cu  | Inner4Cu  | Inner5Cu
                  | Inner6Cu  | Inner7Cu  | Inner8Cu  | Inner9Cu  | Inner10Cu
                  | Inner11Cu | Inner12Cu | Inner13Cu | Inner14Cu | Inner15Cu
                  | Inner16Cu | Inner17Cu | Inner18Cu | Inner19Cu | Inner20Cu
                  | Inner21Cu | Inner22Cu | Inner23Cu | Inner24Cu | Inner25Cu
                  | Inner26Cu | Inner27Cu | Inner28Cu | Inner29Cu | Inner30Cu
                  | Inner31Cu | Inner32Cu
    deriving (Show, Eq, Enum, Bounded)

strToLayerMap :: [(String, PcbnewLayerT)]
strToLayerMap =
    [ ("F.SilkS"   , FSilkS )
    , ("F.Cu"      , FCu    )
    , ("F.Paste"   , FPaste )
    , ("F.Mask"    , FMask  )
    , ("B.SilkS"   , BSilkS )
    , ("B.Cu"      , BCu    )
    , ("B.Paste"   , BPaste )
    , ("B.Mask"    , BMask  )
    , ("Dwgs.User" , DwgsUser)
    , ("Cmts.User" , CmtsUser)
    , ("F.Adhes"   , FAdhes)
    , ("B.Adhes"   , BAdhes)
    , ("F&B.Cu"    , FandBCu)
    , ("*.Cu"      , AllCu  )
    , ("*.Mask"    , AllMask)
    , ("*.SilkS"   , AllSilk)
    , ("*.Paste"   , AllPaste)
    , ("F.CrtYd"   , FCrtYd)
    , ("B.CrtYd"   , BCrtYd)
    , ("F.Fab"     , FFab)
    , ("B.Fab"     , BFab)
    , ("*.Fab"     , AllFab)
    , ("Edge.Cuts" , EdgeCuts)
    , ("Margin"    , Margin)
    , ("Eco1.User" , Eco1User)
    , ("Eco2.User" , Eco2User)
    , ("Inner1.Cu" , Inner1Cu)
    , ("Inner2.Cu" , Inner2Cu)
    , ("Inner3.Cu" , Inner3Cu)
    , ("Inner4.Cu" , Inner4Cu)
    , ("Inner5.Cu" , Inner5Cu)
    , ("Inner6.Cu" , Inner6Cu)
    , ("Inner7.Cu" , Inner7Cu)
    , ("Inner8.Cu" , Inner8Cu)
    , ("Inner9.Cu" , Inner9Cu)
    , ("Inner10.Cu", Inner10Cu)
    , ("Inner11.Cu", Inner11Cu)
    , ("Inner12.Cu", Inner12Cu)
    , ("Inner13.Cu", Inner13Cu)
    , ("Inner14.Cu", Inner14Cu)
    , ("Inner15.Cu", Inner15Cu)
    , ("Inner16.Cu", Inner16Cu)
    , ("Inner17.Cu", Inner17Cu)
    , ("Inner18.Cu", Inner18Cu)
    , ("Inner19.Cu", Inner19Cu)
    , ("Inner20.Cu", Inner20Cu)
    , ("Inner21.Cu", Inner21Cu)
    , ("Inner22.Cu", Inner22Cu)
    , ("Inner23.Cu", Inner23Cu)
    , ("Inner24.Cu", Inner24Cu)
    , ("Inner25.Cu", Inner25Cu)
    , ("Inner26.Cu", Inner26Cu)
    , ("Inner27.Cu", Inner27Cu)
    , ("Inner28.Cu", Inner28Cu)
    , ("Inner29.Cu", Inner29Cu)
    , ("Inner30.Cu", Inner30Cu)
    , ("Inner31.Cu", Inner31Cu)
    , ("Inner32.Cu", Inner32Cu)
    ]

strToLayer :: String -> Maybe PcbnewLayerT
strToLayer s = lookup s strToLayerMap

layerToStr :: PcbnewLayerT -> String
layerToStr l = fromMaybe "" $ lookup l $ map swap strToLayerMap

itemsOn :: [PcbnewLayerT] -> [PcbnewItem] -> [PcbnewItem]
itemsOn = foldMap itemsOn'
    where itemsOn' :: PcbnewLayerT -> [PcbnewItem] -> [PcbnewItem]
          itemsOn' layer = filter ((layer `elem`) . view itemLayers)

data PcbnewPadTypeT = ThruHole | SMD | Connect | NPThruHole
    deriving (Show, Eq, Enum, Bounded)

strToPadTypeMap :: [(String, PcbnewPadTypeT)]
strToPadTypeMap =
    [ ("smd"          , SMD)
    , ("thru_hole"    , ThruHole)
    , ("connect"      , Connect)
    , ("np_thru_hole" , NPThruHole)
    ]

strToPadType :: String -> Maybe PcbnewPadTypeT
strToPadType s = lookup s strToPadTypeMap

fpPadTypeToStr :: PcbnewPadTypeT -> String
fpPadTypeToStr t = fromMaybe "" $ lookup t $ map swap strToPadTypeMap

data PcbnewPadShapeT = Circle | Oval | Rect | Trapezoid | RoundRect
    deriving (Show, Eq, Enum, Bounded)

strToPadShapeMap :: [(String, PcbnewPadShapeT)]
strToPadShapeMap = [ ("circle"   , Circle)
                   , ("oval"     , Oval)
                   , ("rect"     , Rect)
                   , ("roundrect", RoundRect)
                   , ("trapezoid", Trapezoid)
                   ]

strToPadShape :: String -> Maybe PcbnewPadShapeT
strToPadShape s = lookup s strToPadShapeMap

fpPadShapeToStr :: PcbnewPadShapeT -> String
fpPadShapeToStr t = fromMaybe "" $ lookup t $ map swap strToPadShapeMap

data PcbnewJustifyT =
    JustifyLeft | JustifyRight | JustifyTop | JustifyBottom | JustifyMirror
        deriving (Show, Eq, Enum, Bounded)


strToJustifyMap :: [(String, PcbnewJustifyT)]
strToJustifyMap =
    [ ("left"  , JustifyLeft)
    , ("right" , JustifyRight)
    , ("top"   , JustifyTop)
    , ("bottom", JustifyBottom)
    , ("mirror", JustifyMirror)
    ]

strToJustify :: String -> Maybe PcbnewJustifyT
strToJustify s = lookup s strToJustifyMap

justifyToString :: PcbnewJustifyT -> String
justifyToString t = fromMaybe "" $ lookup t $ map swap strToJustifyMap

data PcbnewAtT = PcbnewAtT { pcbnewAtPoint :: V2Double
                           , pcbnewAtOrientation :: Double
                           }
    deriving (Show, Eq)

instance AEq PcbnewAtT where
    (PcbnewAtT p1 o1) ~== (PcbnewAtT p2 o2) = p1 ~== p2 && o1 ~== o2

defaultPcbnewAtT :: PcbnewAtT
defaultPcbnewAtT = PcbnewAtT { pcbnewAtPoint = (0,0)
                             , pcbnewAtOrientation = 0
                             }

fpTextJustify :: Functor f => LensLike' f PcbnewItem [PcbnewJustifyT]
fpTextJustify f (PcbnewFpText t s a l h si th i j) =
    (\j' -> PcbnewFpText t s a l h si th i j') `fmap` f j
fpTextJustify f x = (\_ -> x) `fmap` f []


atP :: Functor f => LensLike' f PcbnewAtT V2Double
atP f (PcbnewAtT p o) =  (\p' -> PcbnewAtT p' o) `fmap` f p

atX :: Functor f => LensLike' f PcbnewAtT Double
atX f (PcbnewAtT (x,y) o) = (\x' -> PcbnewAtT (x',y) o) `fmap` f x

atY :: Functor f => LensLike' f PcbnewAtT Double
atY f (PcbnewAtT (x,y) o) = (\y' -> PcbnewAtT (x,y') o) `fmap` f y

data PcbnewFpTextTypeT = FpTextReference | FpTextValue | FpTextUser
    deriving (Show, Eq, Enum, Bounded)


strToFpTextTypeMap :: [(String, PcbnewFpTextTypeT)]
strToFpTextTypeMap =
    [ ("reference", FpTextReference)
    , ("value"    , FpTextValue)
    , ("user"     , FpTextUser)
    ]

strToFpTextType :: String -> Maybe PcbnewFpTextTypeT
strToFpTextType s = lookup s strToFpTextTypeMap

fpTextTypeToStr :: PcbnewFpTextTypeT -> String
fpTextTypeToStr t = fromMaybe "" $ lookup t $ map swap strToFpTextTypeMap

type V2Double = (Double, Double)

instance Num V2Double where
    (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    (-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
    (*) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
    abs (x,y)     = (abs x, abs y)
    signum (x,y)  = (signum x, signum y)
    fromInteger i = (fromInteger i, fromInteger i)
