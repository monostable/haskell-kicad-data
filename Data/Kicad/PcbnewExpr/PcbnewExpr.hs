{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Kicad.PcbnewExpr.PcbnewExpr
(
-- * Types
  PcbnewAttribute(..)
, PcbnewExpr(..)
, PcbnewFootprint(..)
, PcbnewGrItem(..)
, PcbnewItem(..)
, PcbnewModule(..)
-- * Attribute types
, PcbnewAnchorT(..)
, PcbnewAtT(..)
, PcbnewClearanceT(..)
, PcbnewDrillT(..)
, PcbnewFpTextTypeT(..)
, PcbnewJustifyT(..)
, PcbnewLayerT(..)
, PcbnewPadShapeT(..)
, PcbnewPadTypeT(..)
, PcbnewXyzT
, V2Double
-- * Lenses and other getters/setters
, atP
, atX
, atY
, footprintAttrs
, footprintItems
, fpTextJustify
, itemHandle
, itemLayers
, itemPoints
, itemsOn
, moduleAttrs
, moduleItems
, padAttributes
-- * String conversion
, anchorToStr
, attrFootrintTypeToStr
, clearanceToStr
, fpPadShapeToStr
, fpPadTypeToStr
, fpTextTypeToStr
, justifyToString
, layerToStr
, strToAnchor
, strToAttrFootprintType
, strToClearance
, strToFpTextType
, strToJustify
, strToLayer
, strToPadShape
, strToPadType
, strToZoneConnect
, zoneConnectToStr
-- * Default (empty) instances
, defaultPcbnewAtT
, defaultPcbnewAttr
, defaultPcbnewDrillT
, defaultPcbnewFont
, defaultPcbnewFootprint
, defaultPcbnewFpArc
, defaultPcbnewFpCircle
, defaultPcbnewFpLine
, defaultPcbnewFpPoly
, defaultPcbnewFpRect
, defaultPcbnewFpText
, defaultPcbnewGrArc
, defaultPcbnewGrLine
, defaultPcbnewGrPoly
, defaultPcbnewGroup
, defaultPcbnewModel
, defaultPcbnewModule
, defaultPcbnewPad
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
                | PcbnewExprFootprint PcbnewFootprint
                | PcbnewExprItem PcbnewItem
                | PcbnewExprGrItem PcbnewGrItem
                | PcbnewExprAttribute PcbnewAttribute
    deriving (Show, Eq)

instance AEq PcbnewExpr where
    PcbnewExprModule    x ~== PcbnewExprModule    y = x ~== y
    PcbnewExprFootprint x ~== PcbnewExprFootprint y = x ~== y
    PcbnewExprItem      x ~== PcbnewExprItem      y = x ~== y
    PcbnewExprGrItem    x ~== PcbnewExprGrItem    y = x ~== y
    PcbnewExprAttribute x ~== PcbnewExprAttribute y = x ~== y
    _ ~== _ = False

instance SExpressable PcbnewExpr where
    toSExpr (PcbnewExprModule x)    = toSExpr x
    toSExpr (PcbnewExprFootprint x) = toSExpr x
    toSExpr (PcbnewExprItem x)      = toSExpr x
    toSExpr (PcbnewExprGrItem x)    = toSExpr x
    toSExpr (PcbnewExprAttribute x) = toSExpr x

data PcbnewModule = PcbnewModule { pcbnewModuleName  :: String
                                 , pcbnewModuleLayer :: PcbnewLayerT
                                 , pcbnewModuleAttrs :: [PcbnewAttribute]
                                 , pcbnewModuleItems :: [PcbnewItem]
                                 }
    deriving (Show, Eq)

data PcbnewFootprint = PcbnewFootprint { pcbnewFootprintName      :: String
                                       , pcbnewFootprintVersion   :: String
                                       , pcbnewFootprintGenerator :: String
                                       , pcbnewFootprintLayer     :: PcbnewLayerT
                                       , pcbnewFootprintAttrs     :: [PcbnewAttribute]
                                       , pcbnewFootprintItems     :: [PcbnewItem]
                                       }
    deriving (Show, Eq)


pos :: SourcePos
pos = newPos "" 0 0

instance SExpressable PcbnewModule where
    toSExpr (PcbnewModule name l attrs items) =
        List pos $ [ Atom pos "module"
               , Atom pos name
               , toSExpr (PcbnewLayer l)
               ] ++ map toSExpr attrs
               ++ map toSExpr items

instance SExpressable PcbnewFootprint where
    toSExpr (PcbnewFootprint name ver gen l attrs items) =
        List pos $ [ Atom pos "footprint"
               , Atom pos name
               , toSExpr (PcbnewVersion ver)
               , toSExpr (PcbnewGenerator gen)
               , toSExpr (PcbnewLayer l)
               ] ++ map toSExpr attrs
               ++ map toSExpr items

defaultPcbnewModule :: PcbnewModule
defaultPcbnewModule = PcbnewModule "" FCu [] []

defaultPcbnewFootprint :: PcbnewFootprint
defaultPcbnewFootprint = PcbnewFootprint "" "" "haskell-kicad-data" FCu [] []

moduleItems :: Functor f => LensLike' f PcbnewModule [PcbnewItem]
moduleItems f (PcbnewModule n l a i) = PcbnewModule n l a `fmap` f i

footprintItems :: Functor f => LensLike' f PcbnewFootprint [PcbnewItem]
footprintItems f (PcbnewFootprint n v g l a i) = PcbnewFootprint n v g l a `fmap` f i

moduleAttrs :: Functor f => LensLike' f PcbnewModule [PcbnewAttribute]
moduleAttrs f (PcbnewModule n l a i) = (\a' -> PcbnewModule n l a' i) `fmap` f a

footprintAttrs :: Functor f => LensLike' f PcbnewFootprint [PcbnewAttribute]
footprintAttrs f (PcbnewFootprint n v g l a i) = (\a' -> PcbnewFootprint n v g l a' i) `fmap` f a

instance AEq PcbnewModule where
    PcbnewModule n1 l1 as1 is1 ~== PcbnewModule n2 l2 as2 is2 =
           n1   == n2
        && l1   == l2
        && as1 ~== as2
        && is1 ~== is2

instance AEq PcbnewFootprint where
    PcbnewFootprint n1 v1 g1 l1 as1 is1 ~== PcbnewFootprint n2 v2 g2 l2 as2 is2 =
           n1   == n2
        && v1   == v2
        && g2   == g2
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
                               , itemTstamp      :: String
                               }
                | PcbnewFpLine { itemStart  :: V2Double
                               , itemEnd    :: V2Double
                               , itemLayer  :: PcbnewLayerT
                               , itemWidth  :: Double
                               , itemTstamp :: String
                               }
                | PcbnewFpCircle { itemStart  :: V2Double
                                 , itemEnd    :: V2Double
                                 , itemLayer  :: PcbnewLayerT
                                 , itemWidth  :: Double
                                 , itemFill   :: Maybe Bool
                                 , itemTstamp :: String
                                 }
                | PcbnewFpRect { itemStart  :: V2Double
                               , itemEnd    :: V2Double
                               , itemLayer  :: PcbnewLayerT
                               , itemWidth  :: Double
                               , itemFill   :: Maybe Bool
                               , itemTstamp :: String
                               }
                | PcbnewFpArc { itemStart  :: V2Double
                              , itemEnd    :: V2Double
                              , fpArcAngle :: Double
                              , itemLayer  :: PcbnewLayerT
                              , itemWidth  :: Double
                              , itemTstamp :: String
                              }
                | PcbnewFpPoly { fpPolyPts  :: [V2Double]
                               , itemLayer  :: PcbnewLayerT
                               , itemWidth  :: Double
                               , itemFill   :: Maybe Bool
                               , itemTstamp :: String
                               }
                | PcbnewPad { padNumber      :: String
                            , padType        :: PcbnewPadTypeT
                            , padShape       :: PcbnewPadShapeT
                            , itemAt         :: PcbnewAtT
                            , itemSize       :: V2Double
                            , padLayers      :: [PcbnewLayerT]
                            , itemTstamp     :: String
                            , padAttributes_ :: [PcbnewAttribute]
                            }
                | PcbnewGroup { groupName :: String
                              , groupId   :: String
                              , groupMembers :: [String]
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
    toSExpr (PcbnewFpText t s a l h si th i j ts) =
        List pos $ [ Atom pos "fp_text"
               , Atom pos $ fpTextTypeToStr t
               , Atom pos s
               , toSExpr (PcbnewAt a)
               , toSExpr (PcbnewLayer l)
               , toSExpr (PcbnewTstamp ts)
               ]
               ++ [Atom pos "hide" | h]
               ++ [toSExpr (PcbnewFpTextEffects ([PcbnewFont si th i] ++ (if j == [] then [] else [PcbnewJustify j])))]
    toSExpr (PcbnewFpLine s e l w ts) =
        List pos $ [ Atom pos "fp_line"
             , toSExpr (PcbnewStart s)
             , toSExpr (PcbnewEnd   e)
             , toSExpr (PcbnewLayer l)
             , toSExpr (PcbnewWidth w)
             ] ++ (if ts == "" then [] else [toSExpr (PcbnewTstamp ts)])
    toSExpr (PcbnewFpCircle s e l w fill ts) =
        List pos $ [ Atom pos "fp_circle"
             , toSExpr (PcbnewCenter s)
             , toSExpr (PcbnewEnd    e)
             , toSExpr (PcbnewLayer  l)
             , toSExpr (PcbnewWidth  w)
             ] ++ (maybeToList (fmap (toSExpr . PcbnewShapeFill) fill))
             ++ if ts == "" then [] else [toSExpr (PcbnewTstamp ts)]
    toSExpr (PcbnewFpRect s e l w fill ts) =
        List pos $ [ Atom pos "fp_rect"
             , toSExpr (PcbnewCenter s)
             , toSExpr (PcbnewEnd    e)
             , toSExpr (PcbnewLayer  l)
             , toSExpr (PcbnewWidth  w)
             ] ++ (maybeToList (fmap (toSExpr . PcbnewShapeFill) fill))
             ++ if ts == "" then [] else [toSExpr (PcbnewTstamp ts)]
    toSExpr (PcbnewFpArc s e a l w ts) =
        List pos $ [ Atom pos "fp_arc"
             , toSExpr (PcbnewStart s)
             , toSExpr (PcbnewEnd   e)
             , toSExpr (PcbnewAngle a)
             , toSExpr (PcbnewLayer l)
             , toSExpr (PcbnewWidth w)
             ] ++ if ts == "" then [] else [toSExpr (PcbnewTstamp ts)]
    toSExpr (PcbnewFpPoly ps l w fill ts) =
        List pos $ [ Atom pos "fp_poly"
             , toSExpr (PcbnewPts ps)
             , toSExpr (PcbnewLayer l)
             , toSExpr (PcbnewWidth w)
             ] ++ (maybeToList (fmap (toSExpr . PcbnewShapeFill) fill))
             ++ if ts == "" then [] else [toSExpr (PcbnewTstamp ts)]
    toSExpr (PcbnewPad n t s a si l ts attrs) =
        List pos $ [ Atom pos "pad"
               , Atom pos n
               , Atom pos $ fpPadTypeToStr t
               , Atom pos $ fpPadShapeToStr s
               , toSExpr $ PcbnewAt a
               , toSExpr $ PcbnewSize si
               , toSExpr $ PcbnewLayers l
               ] ++ if ts == "" then [] else [toSExpr (PcbnewTstamp ts)]
                 ++ map toSExpr attrs
    toSExpr (PcbnewGroup n i ms) =
        List pos $ [ Atom pos "group"
                   , toSExpr (PcbnewId i)
                   , toSExpr (PcbnewMembers ms)
                   ]

itemLayers :: Functor f => LensLike' f PcbnewItem [PcbnewLayerT]
itemLayers f item@(PcbnewPad { }) =
    (\ls -> item {padLayers = ls}) `fmap` f (padLayers item)
itemLayers f item = update `fmap` f [itemLayer item]
    where update [] = item
          update ls = item {itemLayer = head ls}

padAttributes :: Functor f => LensLike' f PcbnewItem [PcbnewAttribute]
padAttributes f i = (\as -> i {padAttributes_ = as}) `fmap` f (padAttributes_ i)

instance AEq PcbnewItem where
    (PcbnewFpText t1 s1 a1 l1 h1 si1 th1 i1 j1 ts1)
        ~== (PcbnewFpText t2 s2 a2 l2 h2 si2 th2 i2 j2 ts2) =
           t1   == t2
        && s1   == s2
        && a1  ~== a2
        && l1   == l2
        && h1   == h2
        && si1 ~== si2
        && th1 ~== th2
        && i1   == i2
        && j1   == j2
        && ts1  == ts2
    (PcbnewFpLine s1 e1 l1 w1 ts1) ~== (PcbnewFpLine s2 e2 l2 w2 ts2) =
           s1 ~== s2
        && e1 ~== e2
        && l1  == l2
        && w1 ~== w2
        && ts1  == ts2
    (PcbnewFpCircle s1 e1 l1 w1 f1 ts1) ~== (PcbnewFpCircle s2 e2 l2 w2 f2 ts2) =
           s1 ~== s2
        && e1 ~== e2
        && l1  == l2
        && w1 ~== w2
        && f1 == f2
        && ts1  == ts2
    (PcbnewFpArc s1 e1 a1 l1 w1 ts1) ~== (PcbnewFpArc s2 e2 a2 l2 w2 ts2) =
           s1 ~== s2
        && e1 ~== e2
        && a1 ~== a2
        && l1  == l2
        && w1 ~== w2
        && ts1  == ts2
    (PcbnewFpPoly ps1 l1 w1 f1 ts1) ~== (PcbnewFpPoly ps2 l2 w2 f2 ts2) =
           ps1 ~== ps2
        && l1   == l2
        && w1  ~== w2
        && f1  == f2
        && ts1  == ts2
    (PcbnewPad n1 t1 s1 a1 si1 l1 ts1 attrs1)
        ~== (PcbnewPad n2 t2 s2 a2 si2 l2 ts2 attrs2) =
           n1   == n2
        && t1   == t2
        && s1   == s2
        && a1  ~== a2
        && si1 ~== si2
        && l1   == l2
        && ts1  == ts2
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
                                   , itemTstamp      = ""
                                   }

defaultPcbnewFpLine :: PcbnewItem
defaultPcbnewFpLine = PcbnewFpLine { itemStart  = (0,0)
                                   , itemEnd    = (0,0)
                                   , itemLayer  = FSilkS
                                   , itemWidth  = 0.15
                                   , itemTstamp = ""
                                   }

defaultPcbnewFpCircle :: PcbnewItem
defaultPcbnewFpCircle = PcbnewFpCircle { itemStart    = (0,0)
                                       , itemEnd      = (0,0)
                                       , itemLayer    = FSilkS
                                       , itemWidth    = 0.15
                                       , itemFill     = Nothing
                                       , itemTstamp   = ""
                                       }

defaultPcbnewFpRect :: PcbnewItem
defaultPcbnewFpRect = PcbnewFpRect { itemStart  = (0,0)
                                   , itemEnd    = (0,0)
                                   , itemLayer  = FSilkS
                                   , itemWidth  = 0.15
                                   , itemFill   = Nothing
                                   , itemTstamp = ""
                                   }

defaultPcbnewFpArc :: PcbnewItem
defaultPcbnewFpArc = PcbnewFpArc { itemStart    = (0,0)
                                 , itemEnd      = (0,0)
                                 , fpArcAngle   = 0
                                 , itemLayer    = FSilkS
                                 , itemWidth    = 0.15
                                 , itemTstamp   = ""
                                 }

defaultPcbnewFpPoly :: PcbnewItem
defaultPcbnewFpPoly = PcbnewFpPoly { fpPolyPts   = []
                                   , itemLayer   = FSilkS
                                   , itemWidth   = 0.15
                                   , itemFill    = Nothing
                                   , itemTstamp  = ""
                                   }

defaultPcbnewGroup :: PcbnewItem
defaultPcbnewGroup = PcbnewGroup { groupName = ""
                                 , groupId = ""
                                 , groupMembers = []
                                 }

defaultPcbnewPad :: PcbnewItem
defaultPcbnewPad = PcbnewPad { padNumber      = ""
                             , padType        = ThruHole
                             , padShape       = Circle
                             , itemAt         = defaultPcbnewAtT
                             , itemSize       = (0,0)
                             , padLayers      = []
                             , itemTstamp     = ""
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


data PcbnewGrItem = PcbnewGrPoly { grPolyPoints :: [V2Double]
                                 , grItemWidth  :: Double
                                 , grItemFill   :: Bool
                                 , grItemTstamp :: String
                                 }
                  | PcbnewGrArc  { grArcStart   :: V2Double
                                 , grArcMid     :: Maybe V2Double
                                 , grArcEnd     :: V2Double
                                 , grArcAngle   :: Maybe Double
                                 , grItemWidth  :: Double
                                 , grItemTstamp :: String
                                 }
                  | PcbnewGrLine  { grLineStart  :: V2Double
                                  , grLineEnd    :: V2Double
                                  , grItemWidth  :: Double
                                  , grItemTstamp :: String
                                  }
                  | PcbnewGrCircle  { grCircleCenter :: V2Double
                                    , grCircleEnd    :: V2Double
                                    , grItemWidth    :: Double
                                    , grItemTstamp   :: String
                                    }
      deriving (Show, Eq)

instance AEq PcbnewGrItem where
  PcbnewGrPoly pts1 w1 f1 t1 ~== PcbnewGrPoly pts2 w2 f2 t2
    = pts1 ~== pts2 && w1 ~== w2 && f1 == f2 && t1 == t2
  PcbnewGrArc s1 m1 e1 a1 w1 t1 ~== PcbnewGrArc s2 m2 e2 a2 w2 t2
    = s1 ~== s2 && m1 ~== m2 && e1 ~== e2 && a1 ~== a2 && w1 ~== w2 && t1 == t2
  PcbnewGrLine s1 e1 w1 t1 ~== PcbnewGrLine s2 e2 w2 t2
    = s1 ~== s2 && e1 ~== e2 && w1 ~== w2 && t1 == t2
  PcbnewGrCircle s1 e1 w1 t1 ~== PcbnewGrCircle s2 e2 w2 t2
    = s1 ~== s2 && e1 ~== e2 && w1 ~== w2 && t1 == t2

instance SExpressable PcbnewGrItem where
    toSExpr (PcbnewGrPoly pts w f ts) = List pos $
        [ Atom pos "gr_poly"
        , toSExpr (PcbnewPts pts)
        , toSExpr (PcbnewWidth w)
        ] ++ (if f then [toSExpr PcbnewGrItemFill] else [])
        ++ if ts == "" then [] else [toSExpr (PcbnewTstamp ts)]
    toSExpr (PcbnewGrArc s m e a w ts) = List pos $
        [ Atom pos "gr_arc"
        , toSExpr (PcbnewStart s)
        ] ++ fmap (toSxDD "mid") (maybeToList m)
        ++ [toSExpr (PcbnewEnd e)]
        ++ fmap (toSxD "angle") (maybeToList a)
        ++ [toSExpr (PcbnewWidth w)]
        ++ if ts == "" then [] else [toSExpr (PcbnewTstamp ts)]
    toSExpr (PcbnewGrLine s e w ts) = List pos $
        [ Atom pos "gr_line"
        , toSExpr (PcbnewStart s)
        , toSExpr (PcbnewEnd e)
        , toSExpr (PcbnewWidth w)]
        ++ if ts == "" then [] else [toSExpr (PcbnewTstamp ts)]
    toSExpr (PcbnewGrCircle c e w ts) = List pos $
        [ Atom pos "gr_circle"
        , toSExpr (PcbnewCenter c)
        , toSExpr (PcbnewEnd e)
        , toSExpr (PcbnewWidth w)]
        ++ if ts == "" then [] else [toSExpr (PcbnewTstamp ts)]

defaultPcbnewGrPoly :: PcbnewGrItem
defaultPcbnewGrPoly = PcbnewGrPoly [] 0 False ""

defaultPcbnewGrArc :: PcbnewGrItem
defaultPcbnewGrArc = PcbnewGrArc (0,0) Nothing (0,0) Nothing 0 ""

defaultPcbnewGrLine :: PcbnewGrItem
defaultPcbnewGrLine = PcbnewGrLine (0,0) (0,0) 0 ""

data PcbnewAttribute = PcbnewLayer      PcbnewLayerT
                     | PcbnewAt         PcbnewAtT
                     | PcbnewGenerator  String
                     | PcbnewVersion    String
                     | PcbnewTstamp     String
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
                     | PcbnewAttr { pcbnewAttrFootprintType :: Maybe PcbnewAttrFootprintType
                                  , pcbnewAttrBoardOnly :: Bool
                                  , pcbnewAttrExcludeFromPos :: Bool
                                  , pcbnewAttrExcludeFromBom :: Bool
                                  }
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
                                   , pcbnewModelHide   :: Bool
                                   }
                     | PcbnewModelAt           PcbnewAttribute
                     | PcbnewModelScale        PcbnewAttribute
                     | PcbnewModelRotate       PcbnewAttribute
                     | PcbnewModelOffset       PcbnewAttribute
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
                     | PcbnewZoneConnect       PcbnewZoneConnectT
                     | PcbnewThermalWidth      Double
                     | PcbnewThermalGap        Double
                     | PcbnewJustify           [PcbnewJustifyT]
                     | PcbnewDieLength         Double
                     | PcbnewShapeFill         Bool
                     | PcbnewId                String
                     | PcbnewMembers           [String]
                     | PcbnewOptions { pcbnewOptionsClearance :: PcbnewClearanceT
                                     , pcbnewOptionsAnchor    :: PcbnewAnchorT
                                     }
                     | PcbnewOptionsClearance PcbnewClearanceT
                     | PcbnewOptionsAnchor    PcbnewAnchorT
                     | PcbnewPrimitives       [PcbnewGrItem]
                     | PcbnewGrItemFill
    deriving (Show, Eq)


type PcbnewXyzT = (Double, Double, Double)

instance SExpressable PcbnewAttribute where
    toSExpr (PcbnewLayer l) = List pos [ Atom pos "layer"
                                   , Atom pos $ layerToStr l
                                   ]
    toSExpr (PcbnewAt (PcbnewAtT (x,y) o unlocked)) =
        List pos $ [ Atom pos "at"
               , atomDbl x
               , atomDbl y
               ] ++ [atomDbl o | o /= 0]
               ++ [Atom pos "unlocked" | unlocked]
    toSExpr (PcbnewLayers ls) =
        List pos (Atom pos "layers" : map (Atom pos . layerToStr) ls)
    toSExpr (PcbnewFont s t i) =
        List pos $ [ Atom pos "font", toSExpr (PcbnewSize s)
               , toSExpr (PcbnewThickness t)
               ] ++ [Atom pos "italic" | i]
    toSExpr (PcbnewPts xys) =
        List pos $ Atom pos "pts" : map (toSExpr . PcbnewXy) xys
    toSExpr (PcbnewModel p a s r h) =
        List pos $ [ Atom pos "model"
                   , Atom pos p
                   ] ++ if h then [Atom pos "hide"] else []
                   ++ [ toSExpr (PcbnewModelAt     (PcbnewXyz a))
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
    toSExpr (PcbnewModelOffset xyz)  = List pos [Atom pos "offset", toSExpr xyz]
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
    toSExpr (PcbnewTstamp s)           = toSxStr "tstamp"    s
    toSExpr (PcbnewVersion s)          = toSxStr "version"   s
    toSExpr (PcbnewGenerator s)        = toSxStr "generator" s
    toSExpr (PcbnewTedit s)            = toSxStr "tedit"     s
    toSExpr (PcbnewDescr s)            = toSxStr "descr"     s
    toSExpr (PcbnewTags  s)            = toSxStr "tags"      s
    toSExpr (PcbnewPath  s)            = toSxStr "path"      s
    toSExpr PcbnewItalic               = Atom pos "italic"
    toSExpr PcbnewHide                 = Atom pos "hide"
    toSExpr PcbnewPlaced               = Atom pos "placed"
    toSExpr PcbnewLocked               = Atom pos "locked"
    toSExpr (PcbnewAutoplaceCost90  i) =
        List pos [Atom pos "autoplace_cost90"  , Atom pos (show i)]
    toSExpr (PcbnewAutoplaceCost180 i) =
        List pos [Atom pos "autoplace_cost180" , Atom pos (show i)]
    toSExpr (PcbnewZoneConnect      zc) =
        List pos [Atom pos "zone_connect"      , Atom pos (zoneConnectToStr zc)]
    toSExpr (PcbnewJustify         js) =
        List pos $ (Atom pos "justify"):map (Atom pos . justifyToString) js
    toSExpr (PcbnewShapeFill b)        = List pos [Atom pos "fill", Atom pos (if b then "solid" else "none")]
    toSExpr (PcbnewId s)               = List pos [Atom pos "id", Atom pos s]
    toSExpr (PcbnewMembers ms)         = List pos $ [Atom pos "members"] ++ (fmap (Atom pos) ms)
    toSExpr (PcbnewOptions clr anchr)  = List pos $ [Atom pos "options"
                                                    , toSExpr (PcbnewOptionsClearance clr)
                                                    , toSExpr (PcbnewOptionsAnchor anchr)
                                                    ]
    toSExpr (PcbnewOptionsAnchor a)    = List pos $ [Atom pos "anchor", Atom pos (anchorToStr a)]
    toSExpr (PcbnewOptionsClearance c) = List pos $ [Atom pos "clearance", Atom pos (clearanceToStr c)]
    toSExpr (PcbnewGrItemFill)         = List pos $ [Atom pos "fill", Atom pos "yes"]
    toSExpr (PcbnewPrimitives grs)     = List pos $ [Atom pos "primitives"] ++ fmap toSExpr grs
    toSExpr (PcbnewAttr t bo efpf efb) = List pos $ [ Atom pos "attr" ]
                                            ++ fmap (Atom pos . attrFootrintTypeToStr) (maybeToList t)
                                            ++ if bo then [Atom pos "board_only"] else []
                                            ++ if efpf then [Atom pos "exclude_from_pos_files"] else []
                                            ++ if efb then [Atom pos "exclude_from_bom"] else []
    toSExpr x                          = error $ "toSExpr not implmented for " ++ (show x)


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
    (PcbnewModelOffset       x) ~== (PcbnewModelOffset       y) = x ~== y
    (PcbnewModel p1 a1 s1 r1 h1) ~== (PcbnewModel p2 a2 s2 r2 h2) =
      p1 == p2 && a1 ~== a2 && s1 ~== s2 && r1 ~== r2 && h1 == h2
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
                                 , pcbnewModelHide   = False
                                 }
defaultPcbnewAttr :: PcbnewAttribute
defaultPcbnewAttr = PcbnewAttr { pcbnewAttrFootprintType  = Nothing
                               , pcbnewAttrBoardOnly      = False
                               , pcbnewAttrExcludeFromPos = False
                               , pcbnewAttrExcludeFromBom = False
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

data PcbnewPadShapeT = Circle | Oval | Rect | Trapezoid | RoundRect | Custom
    deriving (Show, Eq, Enum, Bounded)

strToPadShapeMap :: [(String, PcbnewPadShapeT)]
strToPadShapeMap = [ ("circle"   , Circle)
                   , ("oval"     , Oval)
                   , ("rect"     , Rect)
                   , ("roundrect", RoundRect)
                   , ("trapezoid", Trapezoid)
                   , ("custom"  ,  Custom)
                   ]

strToPadShape :: String -> Maybe PcbnewPadShapeT
strToPadShape s = lookup s strToPadShapeMap

fpPadShapeToStr :: PcbnewPadShapeT -> String
fpPadShapeToStr t = fromMaybe "" $ lookup t $ map swap strToPadShapeMap


data PcbnewZoneConnectT = NotConnected | ThermalRelief | Solid | TroughHoleThermalRelief
    deriving (Show, Eq, Enum, Bounded)

strToZoneConnectMap :: [(String, PcbnewZoneConnectT)]
strToZoneConnectMap =
  [ ("0", NotConnected)
  , ("1", ThermalRelief)
  , ("2", Solid)
  , ("3", TroughHoleThermalRelief)
  ]

strToZoneConnect :: String -> Maybe PcbnewZoneConnectT
strToZoneConnect s = lookup s strToZoneConnectMap

zoneConnectToStr :: PcbnewZoneConnectT -> String
zoneConnectToStr t = fromMaybe "" $ lookup t $ map swap strToZoneConnectMap


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
                           , pcbnewAtUnlocked :: Bool
                           }
    deriving (Show, Eq)

instance AEq PcbnewAtT where
  (PcbnewAtT p1 o1 l1) ~== (PcbnewAtT p2 o2 l2) = p1 ~== p2 && o1 ~== o2 && l1 == l2

defaultPcbnewAtT :: PcbnewAtT
defaultPcbnewAtT = PcbnewAtT { pcbnewAtPoint = (0,0)
                             , pcbnewAtOrientation = 0
                             , pcbnewAtUnlocked = False
                             }


fpTextJustify :: Functor f => LensLike' f PcbnewItem [PcbnewJustifyT]
fpTextJustify f (PcbnewFpText t s a l h si th i j ts) =
    (\j' -> PcbnewFpText t s a l h si th i j' ts) `fmap` f j
fpTextJustify f x = (\_ -> x) `fmap` f []


atP :: Functor f => LensLike' f PcbnewAtT V2Double
atP f (PcbnewAtT p o l) =  (\p' -> PcbnewAtT p' o l) `fmap` f p

atX :: Functor f => LensLike' f PcbnewAtT Double
atX f (PcbnewAtT (x,y) o l) = (\x' -> PcbnewAtT (x',y) o l) `fmap` f x

atY :: Functor f => LensLike' f PcbnewAtT Double
atY f (PcbnewAtT (x,y) o l) = (\y' -> PcbnewAtT (x,y') o l) `fmap` f y

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

data PcbnewClearanceT = ClearanceOutline | ClearanceConvexHull
  deriving (Show, Eq, Enum, Bounded)

strToPcbnewClearanceMap :: [(String, PcbnewClearanceT)]
strToPcbnewClearanceMap =
  [ ("outline", ClearanceOutline)
  , ("convexhull", ClearanceConvexHull)
  ]

strToClearance :: String -> Maybe PcbnewClearanceT
strToClearance s = lookup s strToPcbnewClearanceMap

clearanceToStr :: PcbnewClearanceT -> String
clearanceToStr t = fromMaybe "" $ lookup t $ map swap strToPcbnewClearanceMap

data PcbnewAnchorT = AnchorRect | AnchorCircle
  deriving (Show, Eq, Enum, Bounded)


strToPcbnewAnchorMap :: [(String, PcbnewAnchorT)]
strToPcbnewAnchorMap =
  [ ("rect", AnchorRect)
  , ("circle", AnchorCircle)
  ]

strToAnchor :: String -> Maybe PcbnewAnchorT
strToAnchor s = lookup s strToPcbnewAnchorMap

anchorToStr :: PcbnewAnchorT -> String
anchorToStr t = fromMaybe "" $ lookup t $ map swap strToPcbnewAnchorMap

data PcbnewAttrFootprintType = PcbnewAttrSmd | PcbnewAttrThroughHole | PcbnewAttrVirtual
  deriving (Show, Eq, Enum, Bounded)

strToPcbnewAttrFootprintTypeMap :: [(String, PcbnewAttrFootprintType)]
strToPcbnewAttrFootprintTypeMap =
  [ ("smd", PcbnewAttrSmd)
  , ("through_hole", PcbnewAttrThroughHole)
  , ("virtual", PcbnewAttrVirtual)
  ]

strToAttrFootprintType :: String -> Maybe PcbnewAttrFootprintType
strToAttrFootprintType s = lookup s strToPcbnewAttrFootprintTypeMap

attrFootrintTypeToStr :: PcbnewAttrFootprintType -> String
attrFootrintTypeToStr t = fromMaybe "" $ lookup t $ map swap strToPcbnewAttrFootprintTypeMap
