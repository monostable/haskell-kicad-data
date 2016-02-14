{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Kicad.PcbnewExpr.PcbnewExpr
(
-- * Types
  PcbnewExpr(..)
, PcbnewModule(..)
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

import Data.Kicad.SExpr.SExpr
import Data.Kicad.Util

data PcbnewExpr = PcbnewExprModule PcbnewModule
                | PcbnewExprItem PcbnewItem
                | PcbnewExprAttribute PcbnewAttribute
    deriving (Show, Eq)

instance AEq PcbnewExpr where
    PcbnewExprModule    x ~== PcbnewExprModule    y = x ~== y
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

instance SExpressable PcbnewModule where
    toSExpr (PcbnewModule name l attrs items) =
        List $ [ AtomKey KeyModule
               , AtomStr name
               , toSExpr (PcbnewLayer l)
               ] ++ map toSExpr attrs
               ++ map toSExpr items

defaultPcbnewModule :: PcbnewModule
defaultPcbnewModule = PcbnewModule "" FCu [] []

moduleItems :: Functor f => LensLike' f PcbnewModule [PcbnewItem]
moduleItems f (PcbnewModule n l a i) = PcbnewModule n l a `fmap` f i

moduleAttrs :: Functor f => LensLike' f PcbnewModule [PcbnewAttribute]
moduleAttrs f (PcbnewModule n l a i) = (\a' -> PcbnewModule n l a' i) `fmap` f a

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
        List $ [ AtomKey KeyFpText
               , AtomStr $ fpTextTypeToStr t
               , AtomStr s
               , toSExpr (PcbnewAt a)
               , toSExpr (PcbnewLayer l)
               ] ++ [AtomStr "hide" | h] ++
               [toSExpr $ PcbnewFpTextEffects ([PcbnewFont si th i]
                   ++ (fmap PcbnewJustify j))]
    toSExpr (PcbnewFpLine s e l w) =
        List [ AtomKey KeyFpLine
             , toSExpr (PcbnewStart s)
             , toSExpr (PcbnewEnd   e)
             , toSExpr (PcbnewLayer l)
             , toSExpr (PcbnewWidth w)
             ]
    toSExpr (PcbnewFpCircle s e l w) =
        List [ AtomKey KeyFpCircle
             , toSExpr (PcbnewStart s)
             , toSExpr (PcbnewEnd   e)
             , toSExpr (PcbnewLayer l)
             , toSExpr (PcbnewWidth w)
             ]
    toSExpr (PcbnewFpArc s e a l w) =
        List [ AtomKey KeyFpArc
             , toSExpr (PcbnewStart s)
             , toSExpr (PcbnewEnd   e)
             , toSExpr (PcbnewAngle a)
             , toSExpr (PcbnewLayer l)
             , toSExpr (PcbnewWidth w)
             ]
    toSExpr (PcbnewFpPoly ps l w) =
        List [ AtomKey KeyFpPoly
             , toSExpr (PcbnewPts ps)
             , toSExpr (PcbnewLayer l)
             , toSExpr (PcbnewWidth w)
             ]
    toSExpr (PcbnewPad n t s a si l attrs) =
        List $ [ AtomKey KeyPad
               , AtomStr n
               , AtomStr $ fpPadTypeToStr t
               , AtomStr $ fpPadShapeToStr s
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
                     | PcbnewLocked
                     | PcbnewStart      V2Double
                     | PcbnewEnd        V2Double
                     | PcbnewWidth      Double
                     | PcbnewDescr      String
                     | PcbnewTags       String
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
                     | PcbnewCenter            V2Double
                     | PcbnewClearance         Double
                     | PcbnewMaskMargin        Double
                     | PcbnewPasteMargin       Double
                     | PcbnewPasteMarginRatio  Double
                     | PcbnewOffset            V2Double
                     | PcbnewAutoplaceCost90   Int
                     | PcbnewAutoplaceCost180  Int
                     | PcbnewZoneConnect       Int
                     | PcbnewThermalWidth      Double
                     | PcbnewThermalGap        Double
                     | PcbnewJustify           PcbnewJustifyT
    deriving (Show, Eq)



type PcbnewXyzT = (Double, Double, Double)

instance SExpressable PcbnewAttribute where
    toSExpr (PcbnewLayer l) = List [ AtomKey KeyLayer
                                  , AtomStr $ layerToStr l
                                  ]
    toSExpr (PcbnewAt (PcbnewAtT (x,y) o)) =
        List $ [ AtomKey KeyAt
               , AtomDbl x
               , AtomDbl y
               ] ++ [AtomDbl o | o /= 0]
    toSExpr (PcbnewLayers ls) =
        List (AtomKey KeyLayers : map (AtomStr . layerToStr) ls)
    toSExpr (PcbnewFont s t i) =
        List $ [ AtomKey KeyFont, toSExpr (PcbnewSize s)
               , toSExpr (PcbnewThickness t)
               ] ++ [AtomStr "italic" | i]
    toSExpr (PcbnewPts xys) =
        List $ AtomKey KeyPts : map (toSExpr . PcbnewXy) xys
    toSExpr (PcbnewModel p a s r) =
        List [AtomKey KeyModel
             , AtomStr p
             , toSExpr (PcbnewModelAt     (PcbnewXyz a))
             , toSExpr (PcbnewModelScale  (PcbnewXyz s))
             , toSExpr (PcbnewModelRotate (PcbnewXyz r))
             ]
    toSExpr (PcbnewDrill (PcbnewDrillT s o off)) =
        List $ [AtomKey KeyDrill]
             ++ [AtomStr "oval" | o]
             ++ (if o && isJust s
                then [AtomDbl (fst (fromJust s)), AtomDbl (snd (fromJust s))]
                else [AtomDbl (fst (fromJust s)) | isJust s])
             ++ [toSExpr (PcbnewOffset (fromJust off)) | isJust off]
    toSExpr (PcbnewXyz (x,y,z)) =
        List [AtomKey KeyXyz, AtomDbl x, AtomDbl y, AtomDbl z]
    toSExpr (PcbnewFpTextEffects l)  = List $ [AtomKey KeyEffects] ++ fmap toSExpr l
    toSExpr (PcbnewFpTextType t)     = AtomStr $ fpTextTypeToStr t
    toSExpr (PcbnewModelAt     xyz)  = List [AtomKey KeyAt    , toSExpr xyz]
    toSExpr (PcbnewModelScale  xyz)  = List [AtomKey KeyScale , toSExpr xyz]
    toSExpr (PcbnewModelRotate xyz)  = List [AtomKey KeyRotate, toSExpr xyz]
    toSExpr (PcbnewClearance   d) = toSxD KeyClearance              d
    toSExpr (PcbnewMaskMargin  d) = toSxD KeySolderMaskMargin       d
    toSExpr (PcbnewPasteMargin d) = toSxD KeySolderPasteMargin      d
    toSExpr (PcbnewPasteMarginRatio  d) = toSxD KeySolderPasteMarginRatio d
    toSExpr (PcbnewThickness   d) = toSxD KeyThickness              d
    toSExpr (PcbnewWidth       d) = toSxD KeyWidth                  d
    toSExpr (PcbnewAngle       d) = toSxD KeyAngle                  d
    toSExpr (PcbnewThermalWidth d) = toSxD KeyThermalWidth          d
    toSExpr (PcbnewThermalGap   d) = toSxD KeyThermalGap            d
    toSExpr (PcbnewSize      xy)  = toSxDD KeySize      xy
    toSExpr (PcbnewStart     xy)  = toSxDD KeyStart     xy
    toSExpr (PcbnewCenter    xy)  = toSxDD KeyCenter    xy
    toSExpr (PcbnewRectDelta xy)  = toSxDD KeyRectDelta xy
    toSExpr (PcbnewEnd       xy)  = toSxDD KeyEnd       xy
    toSExpr (PcbnewXy        xy)  = toSxDD KeyXy        xy
    toSExpr (PcbnewOffset    xy)  = toSxDD KeyOffset    xy
    toSExpr (PcbnewTedit s)       = toSxStr KeyTedit s
    toSExpr (PcbnewDescr s)       = toSxStr KeyDescr s
    toSExpr (PcbnewTags  s)       = toSxStr KeyTags  s
    toSExpr (PcbnewAttr  s)       = toSxStr KeyAttr  s
    toSExpr PcbnewItalic = AtomStr "italic"
    toSExpr PcbnewHide   = AtomStr "hide"
    toSExpr PcbnewLocked = AtomStr "locked"
    toSExpr (PcbnewAutoplaceCost90  i) = toSxD KeyAutoplaceCost90  (fromIntegral i)
    toSExpr (PcbnewAutoplaceCost180 i) = toSxD KeyAutoplaceCost180 (fromIntegral i)
    toSExpr (PcbnewZoneConnect      i) = toSxD KeyZoneConnect      (fromIntegral i)
    toSExpr (PcbnewJustify          j) = toSxStr KeyJustify (justifyToString j)

toSxD :: Keyword -> Double -> SExpr
toSxD  kw d = List [AtomKey kw, AtomDbl d]

toSxDD :: Keyword -> V2Double -> SExpr
toSxDD kw (x,y) = List [AtomKey kw, AtomDbl x, AtomDbl y]

toSxStr :: Keyword -> String -> SExpr
toSxStr kw s = List [AtomKey kw, AtomStr s]

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
                  | BFab      | Margin    | Eco1User  | Eco2User  | BAdhes
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

data PcbnewPadShapeT = Circle | Oval | Rect | Trapezoid
    deriving (Show, Eq, Enum, Bounded)

strToPadShapeMap :: [(String, PcbnewPadShapeT)]
strToPadShapeMap = [ ("circle"   , Circle)
                   , ("oval"     , Oval)
                   , ("rect"     , Rect)
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
