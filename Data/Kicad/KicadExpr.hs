{-# LANGUAGE FlexibleInstances #-}
module Data.Kicad.KicadExpr where
import Lens.Family2
import Data.AEq
import Data.Tuple (swap)
import Data.Maybe
import Data.Foldable (foldMap)

import Data.Kicad.SExpr

data KicadExpr = KicadExprModule KicadModule
               | KicadExprItem KicadItem
               | KicadExprAttribute KicadAttribute
    deriving (Show, Eq)

--instance SExpressable KicadItem where
--    toSExpr (KicadFpText t s a l h si th i) =
--        List $ [ AtomKey KeyFpText
--               , AtomStr $ fpTextTypeToStr t
--               , toSExpr (KicadAt a)
--               , toSExpr (KicadLayer l)
--               ]--  ++ if h then [AtomStr "hide"] else [] ++ [

instance AEq KicadExpr where
    KicadExprModule    x ~== KicadExprModule    y = x ~== y
    KicadExprItem      x ~== KicadExprItem      y = x ~== y
    KicadExprAttribute x ~== KicadExprAttribute y = x ~== y
    _ ~== _ = False

data KicadModule = KicadModule  { kicadModuleName  :: String
                                , kicadModuleLayer :: KicadLayerT
                                , kicadModuleItems :: [KicadItem]
                                }
    deriving (Show, Eq)

instance AEq KicadModule where
    KicadModule n1 l1 is1 ~== KicadModule n2 l2 is2 =
           n1   == n2
        && l1   == l2
        && is1 ~== is2

data KicadItem = KicadFpText { fpTextType      :: KicadFpTextTypeT
                             , fpTextStr       :: String
                             , itemAt          :: KicadAtT
                             , itemLayer       :: KicadLayerT
                             , fpTextHide      :: Bool
                             , itemSize        :: (Double, Double)
                             , fpTextThickness :: Double
                             , fpTextItalic    :: Bool
                             }
               | KicadFpLine { itemStart :: (Double, Double)
                             , itemEnd   :: (Double, Double)
                             , itemLayer :: KicadLayerT
                             , itemWidth :: Double
                             }
               | KicadFpCircle { itemStart  :: (Double, Double)
                               , itemEnd    :: (Double, Double)
                               , itemLayer  :: KicadLayerT
                               , itemWidth  :: Double
                               }
               | KicadFpArc { itemStart  :: (Double, Double)
                            , itemEnd    :: (Double, Double)
                            , fpArcAngle :: Double
                            , itemLayer  :: KicadLayerT
                            , itemWidth  :: Double
                            }
               | KicadFpPoly { fpPolyPts :: [(Double, Double)]
                             , itemLayer :: KicadLayerT
                             , itemWidth :: Double
                             }
               | KicadPad { padNumber     :: String
                          , padType       :: KicadPadTypeT
                          , padShape      :: KicadPadShapeT
                          , itemAt        :: KicadAtT
                          , itemSize      :: (Double, Double)
                          , padLayers     :: [KicadLayerT]
                          , padDrill      :: Maybe Double
                          , padRectDelta  :: Maybe (Double, Double)
                          }
    deriving (Show, Eq)

itemLayers :: Functor f => LensLike' f KicadItem [KicadLayerT]
itemLayers f item@(KicadPad { }) = (\ls -> item {padLayers = ls}) `fmap` f (padLayers item)
itemLayers f item = update `fmap` f [itemLayer item]
    where update [] = item
          update ls = item {itemLayer = head ls}

instance AEq KicadItem where
    (KicadFpText t1 s1 a1 l1 h1 si1 th1 i1) ~== (KicadFpText t2 s2 a2 l2 h2 si2 th2 i2) =
           t1   == t2
        && s1   == s2
        && a1  ~== a2
        && l1   == l2
        && h1   == h2
        && si1 ~== si2
        && th1 ~== th2
        && i1   == i2
    (KicadFpLine s1 e1 l1 w1) ~== (KicadFpLine s2 e2 l2 w2) =
           s1 ~== s2
        && e1 ~== e2
        && l1  == l2
        && w1 ~== w2
    (KicadFpCircle s1 e1 l1 w1) ~== (KicadFpCircle s2 e2 l2 w2) =
           s1 ~== s2
        && e1 ~== e2
        && l1  == l2
        && w1 ~== w2
    (KicadFpArc s1 e1 a1 l1 w1) ~== (KicadFpArc s2 e2 a2 l2 w2) =
           s1 ~== s2
        && e1 ~== e2
        && a1 ~== a2
        && l1  == l2
        && w1 ~== w2
    (KicadFpPoly ps1 l1 w1) ~== (KicadFpPoly ps2 l2 w2) =
           ps1 ~== ps2
        && l1   == l2
        && w1  ~== w2
    (KicadPad n1 t1 s1 a1 si1 l1 d1 r1) ~== (KicadPad n2 t2 s2 a2 si2 l2 d2 r2) =
           n1   == n2
        && t1   == t2
        && s1   == s2
        && a1  ~== a2
        && si1 ~== si2
        && l1   == l2
        && d1  ~== d2
        && r1  ~== r2
    x ~== y = x == y

defaultKicadFpText :: KicadItem
defaultKicadFpText = KicadFpText { fpTextType      = FpTextUser
                                 , fpTextStr       = ""
                                 , itemAt          = defaultKicadAtT
                                 , itemLayer       = FSilkS
                                 , fpTextHide      = False
                                 , itemSize        = (1.0, 1.0)
                                 , fpTextThickness = 1.0
                                 , fpTextItalic    = False
                                 }

defaultKicadFpLine :: KicadItem
defaultKicadFpLine = KicadFpLine { itemStart = (0,0)
                                 , itemEnd   = (0,0)
                                 , itemLayer = FSilkS
                                 , itemWidth = 0.15
                                 }

defaultKicadFpCircle :: KicadItem
defaultKicadFpCircle = KicadFpCircle { itemStart = (0,0)
                                     , itemEnd   = (0,0)
                                     , itemLayer = FSilkS
                                     , itemWidth = 0.15
                                     }

defaultKicadFpArc :: KicadItem
defaultKicadFpArc = KicadFpArc { itemStart  = (0,0)
                               , itemEnd    = (0,0)
                               , fpArcAngle = 0
                               , itemLayer  = FSilkS
                               , itemWidth = 0.15
                               }
defaultKicadFpPoly :: KicadItem
defaultKicadFpPoly = KicadFpPoly { fpPolyPts   = []
                                 , itemLayer   = FSilkS
                                 , itemWidth = 0.15
                                 }

defaultKicadPad :: KicadItem
defaultKicadPad = KicadPad { padNumber    = ""
                           , padType      = ThruHole
                           , padShape     = Circle
                           , itemAt       = defaultKicadAtT
                           , itemSize     = (0,0)
                           , padLayers    = []
                           , padDrill     = Nothing
                           , padRectDelta = Nothing
                           }

data KicadAttribute = KicadLayer KicadLayerT
                    | KicadAt KicadAtT
                    | KicadFpTextType KicadFpTextTypeT
                    | KicadSize (Double, Double)
                    | KicadThickness Double
                    | KicadTedit String
                    | KicadItalic
                    | KicadHide
                    | KicadStart (Double, Double)
                    | KicadEnd (Double, Double)
                    | KicadWidth Double
                    | KicadDescr String
                    | KicadTags String
                    | KicadAttr String
                    | KicadLayers [KicadLayerT]
                    | KicadDrill Double
                    | KicadRectDelta (Double, Double)
                    | KicadFpTextEffects KicadAttribute
                    | KicadFont { kicadFontSize :: (Double, Double)
                                , kicadFontThickness :: Double
                                , kicadFontItalic :: Bool
                                }
                    | KicadAngle Double
                    | KicadXy (Double, Double)
                    | KicadPts [(Double, Double)]
                    | KicadModel { kicadModelPath   :: String
                                 , kicadModelAt     :: KicadXyzT
                                 , kicadModelScale  :: KicadXyzT
                                 , kicadModelRotate :: KicadXyzT
                                 }
                    | KicadModelAt     KicadAttribute
                    | KicadModelScale  KicadAttribute
                    | KicadModelRotate KicadAttribute
                    | KicadXyz         KicadXyzT
                    | KicadCenter (Double, Double)
    deriving (Show, Eq)

type KicadXyzT = (Double, Double, Double)

instance SExpressable KicadAttribute where
    toSExpr (KicadLayer l)      =
        List [ AtomKey KeyLayer
             , AtomStr $ layerToStr l
             ]
    toSExpr (KicadAt (KicadAtT (x,y) o)) =
        List $ [ AtomKey KeyAt
               , AtomDbl x
               , AtomDbl y
               ] ++ [AtomDbl o | o /= 0]
    toSExpr (KicadFpTextType t)     = AtomStr $ fpTextTypeToStr t
    toSExpr (KicadSize (x,y))       = List [AtomKey KeySize, AtomDbl x, AtomDbl y]
    toSExpr (KicadThickness d)      = List [AtomKey KeyThickness, AtomDbl d]
    toSExpr (KicadTedit s)          = List [AtomKey KeyTedit, AtomStr s]
    toSExpr KicadItalic             = AtomStr "italic"
    toSExpr KicadHide               = AtomStr "hide"
    toSExpr (KicadStart (x,y))      = List [AtomKey KeyStart, AtomDbl x, AtomDbl y]
    toSExpr (KicadEnd   (x,y))      = List [AtomKey KeyEnd  , AtomDbl x, AtomDbl y]
    toSExpr (KicadCenter (x,y))     = List [AtomKey KeyCenter, AtomDbl x, AtomDbl y]
    toSExpr (KicadWidth d)          = List [AtomKey KeyWidth, AtomDbl d]
    toSExpr (KicadDescr s)          = List [AtomKey KeyDescr, AtomStr s]
    toSExpr (KicadTags s)           = List [AtomKey KeyTags , AtomStr s]
    toSExpr (KicadAttr s)           = List [AtomKey KeyAttr , AtomStr s]
    toSExpr (KicadLayers ls)        =
        List (AtomKey KeyLayers : map (AtomStr . layerToStr) ls)
    toSExpr (KicadDrill d)          = List [AtomKey KeyDrill, AtomDbl d]
    toSExpr (KicadRectDelta (x,y))  =
        List [AtomKey KeyRectDelta, AtomDbl x, AtomDbl y]
    toSExpr (KicadFpTextEffects a)  = List [AtomKey KeyEffects, toSExpr a]
    toSExpr (KicadFont s t i)       =
        List $ [ AtomKey KeyFont, toSExpr (KicadSize s)
               , toSExpr (KicadThickness t)
               ] ++ [AtomStr "italic" | i]
    toSExpr (KicadAngle d)          = List [AtomKey KeyAngle, AtomDbl d]
    toSExpr (KicadXy (x,y))         = List [AtomKey KeyXy, AtomDbl x, AtomDbl y]
    toSExpr (KicadPts xys)          =
        List $ [AtomKey KeyPts] ++  map (toSExpr . KicadXy) xys
    toSExpr (KicadModel p a s r)    =
        List [AtomKey KeyModel
             , AtomStr p
             , toSExpr (KicadModelAt     (KicadXyz a))
             , toSExpr (KicadModelScale  (KicadXyz s))
             , toSExpr (KicadModelRotate (KicadXyz r))
             ]
    toSExpr (KicadModelAt     xyz)  = List [AtomKey KeyAt    , toSExpr xyz]
    toSExpr (KicadModelScale  xyz)  = List [AtomKey KeyScale , toSExpr xyz]
    toSExpr (KicadModelRotate xyz)  = List [AtomKey KeyRotate, toSExpr xyz]
    toSExpr (KicadXyz (x,y,z))      =
        List [AtomKey KeyXyz, AtomDbl x, AtomDbl y, AtomDbl z]

instance AEq KicadAttribute where
    (KicadAt        x) ~== (KicadAt        y) = x ~== y
    (KicadSize      x) ~== (KicadSize      y) = x ~== y
    (KicadThickness x) ~== (KicadThickness y) = x ~== y
    (KicadStart     x) ~== (KicadStart     y) = x ~== y
    (KicadEnd       x) ~== (KicadEnd       y) = x ~== y
    (KicadWidth     x) ~== (KicadWidth     y) = x ~== y
    (KicadDrill     x) ~== (KicadDrill     y) = x ~== y
    (KicadRectDelta x) ~== (KicadRectDelta y) = x ~== y
    (KicadAngle     x) ~== (KicadAngle     y) = x ~== y
    (KicadXy        x) ~== (KicadXy        y) = x ~== y
    (KicadPts       x) ~== (KicadPts       y) = x ~== y
    (KicadXyz       x) ~== (KicadXyz       y) = x ~== y
    (KicadModelAt x)         ~== (KicadModelAt y)     = x ~== y
    (KicadModelScale x)      ~== (KicadModelScale y)  = x ~== y
    (KicadModelRotate x)     ~== (KicadModelRotate y) = x ~== y
    (KicadModel p1 a1 s1 r1) ~== (KicadModel p2 a2 s2 r2) =
        p1 == p2 && a1 ~== a2 && s1 ~== s2 && r1 ~== r2
    (KicadFont s1 t1 i1) ~== (KicadFont s2 t2 i2) =
        s1 ~== s2 && t1 ~== t2 && i1 == i2
    x ~== y = x == y

defaultKicadFont :: KicadAttribute
defaultKicadFont = KicadFont { kicadFontSize = (1.0, 1.0)
                             , kicadFontThickness = 1.0
                             , kicadFontItalic = False
                             }

defaultKicadModel :: KicadAttribute
defaultKicadModel = KicadModel { kicadModelPath   = ""
                               , kicadModelAt     = (0,0,0)
                               , kicadModelScale  = (0,0,0)
                               , kicadModelRotate = (0,0,0)
                               }

data KicadLayerT = FSilkS   | FCu | FPaste | FMask
                 | BSilkS   | BCu | BPaste | BMask
                 | DwgsUser
                 | FandBCu  | AllCu  | AllMask
    deriving (Show, Eq, Enum, Bounded)

strToLayerMap :: [(String, KicadLayerT)]
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
    , ("F&B.Cu"    , FandBCu)
    , ("*.Cu"      , AllCu  )
    , ("*.Mask"    , AllMask)
    ]

strToLayer :: String -> Maybe KicadLayerT
strToLayer s = lookup s strToLayerMap

layerToStr :: KicadLayerT -> String
layerToStr l = fromMaybe "" $ lookup l $ map swap strToLayerMap

itemsOn :: [KicadLayerT] -> [KicadItem] -> [KicadItem]
itemsOn = foldMap itemsOn'
    where itemsOn' :: KicadLayerT -> [KicadItem] -> [KicadItem]
          itemsOn' layer = filter ((layer `elem`) . view itemLayers)

data KicadPadTypeT = ThruHole | SMD | Connect | NPThruHole
    deriving (Show, Eq, Enum, Bounded)

data KicadPadShapeT = Circle | Oval | Rect | Trapezoid
    deriving (Show, Eq, Enum, Bounded)

data KicadAtT = KicadAtT { kicadAtPoint :: (Double, Double)
                         , kicadAtOrientation :: Double
                         }
    deriving (Show, Eq)

instance AEq KicadAtT where
    (KicadAtT p1 o1) ~== (KicadAtT p2 o2) = p1 ~== p2 && o1 ~== o2

defaultKicadAtT :: KicadAtT
defaultKicadAtT = KicadAtT { kicadAtPoint = (0,0)
                           , kicadAtOrientation = 0
                           }

data KicadFpTextTypeT = FpTextReference | FpTextValue | FpTextUser
    deriving (Show, Eq, Enum, Bounded)


strToFpTextTypeMap :: [(String, KicadFpTextTypeT)]
strToFpTextTypeMap =
    [ ("reference", FpTextReference)
    , ("value"    , FpTextValue)
    , ("user"     , FpTextUser)
    ]

strToFpTextType :: String -> Maybe KicadFpTextTypeT
strToFpTextType s = lookup s strToFpTextTypeMap

fpTextTypeToStr :: KicadFpTextTypeT -> String
fpTextTypeToStr t = fromMaybe "" $ lookup t $ map swap strToFpTextTypeMap

class SExpressable a where
    toSExpr :: a -> SExpr
