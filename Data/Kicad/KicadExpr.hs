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
                          , padDrill      :: Maybe KicadDrillT
                          , padRectDelta  :: Maybe (Double, Double)
                          , padPasteMargin      :: Maybe Double
                          , padPasteMarginRatio :: Maybe Double
                          , padMaskMargin       :: Maybe Double
                          , padClearance        :: Maybe Double
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
    (KicadPad n1 t1 s1 a1 si1 l1 d1 r1 pm1 pmr1 mm1 c1) ~== (KicadPad n2 t2 s2 a2 si2 l2 d2 r2 pm2 pmr2 mm2 c2) =
           n1   == n2
        && t1   == t2
        && s1   == s2
        && a1  ~== a2
        && si1 ~== si2
        && l1   == l2
        && d1  ~== d2
        && r1  ~== r2
        && pm1   ~== pm2
        && pmr1  ~== pmr2
        && mm1   ~== mm2
        && c1    ~== c2
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
                           , padPasteMargin      = Nothing
                           , padPasteMarginRatio = Nothing
                           , padMaskMargin       = Nothing
                           , padClearance        = Nothing
                           }

data KicadDrillT = KicadDrillT { kicadDrillX :: Maybe Double
                               , kicadDrillY :: Maybe Double
                               , kicadDrillOval :: Bool
                               , kicadDrillOffset :: Maybe (Double, Double)
                               }
    deriving (Show, Eq)

defaultKicadDrillT  = KicadDrillT Nothing Nothing False Nothing

instance AEq KicadDrillT where
    KicadDrillT x1 y1 o1 off1 ~== KicadDrillT x2 y2 o2 off2
        = x1 ~== x2 && y1 ~== y2 && o1 == o2 && off1 ~== off2

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
                    | KicadDrill KicadDrillT
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
                    | KicadClearance   Double
                    | KicadMaskMargin  Double
                    | KicadPasteMargin Double
                    | KicadPasteMarginRatio  Double
                    | KicadOffset (Double, Double)
    deriving (Show, Eq)

type KicadXyzT = (Double, Double, Double)

instance SExpressable KicadAttribute where
    toSExpr (KicadLayer l) = List [ AtomKey KeyLayer
                                  , AtomStr $ layerToStr l
                                  ]
    toSExpr (KicadAt (KicadAtT (x,y) o)) =
        List $ [ AtomKey KeyAt
               , AtomDbl x
               , AtomDbl y
               ] ++ [AtomDbl o | o /= 0]
    toSExpr (KicadLayers ls) =
        List (AtomKey KeyLayers : map (AtomStr . layerToStr) ls)
    toSExpr (KicadFont s t i) =
        List $ [ AtomKey KeyFont, toSExpr (KicadSize s)
               , toSExpr (KicadThickness t)
               ] ++ [AtomStr "italic" | i]
    toSExpr (KicadPts xys) =
        List $ [AtomKey KeyPts] ++  map (toSExpr . KicadXy) xys
    toSExpr (KicadModel p a s r) =
        List [AtomKey KeyModel
             , AtomStr p
             , toSExpr (KicadModelAt     (KicadXyz a))
             , toSExpr (KicadModelScale  (KicadXyz s))
             , toSExpr (KicadModelRotate (KicadXyz r))
             ]
    toSExpr (KicadDrill (KicadDrillT x y o off)) =
        List $ [AtomKey KeyDrill]
             ++ [AtomStr "oval" | o]
             ++ map AtomDbl (maybeToList x)
             ++ map AtomDbl (maybeToList y)
             ++ [toSExpr (KicadOffset (fromJust off)) | isJust off]
    toSExpr (KicadXyz (x,y,z)) =
        List [AtomKey KeyXyz, AtomDbl x, AtomDbl y, AtomDbl z]
    toSExpr (KicadFpTextEffects a)  = List [AtomKey KeyEffects, toSExpr a]
    toSExpr (KicadFpTextType t)     = AtomStr $ fpTextTypeToStr t
    toSExpr (KicadModelAt     xyz)  = List [AtomKey KeyAt    , toSExpr xyz]
    toSExpr (KicadModelScale  xyz)  = List [AtomKey KeyScale , toSExpr xyz]
    toSExpr (KicadModelRotate xyz)  = List [AtomKey KeyRotate, toSExpr xyz]
    toSExpr (KicadClearance   d) = toSxD KeyClearance              d
    toSExpr (KicadMaskMargin  d) = toSxD KeySolderMaskMargin       d
    toSExpr (KicadPasteMargin d) = toSxD KeySolderPasteMargin      d
    toSExpr (KicadPasteMarginRatio  d) = toSxD KeySolderPasteMarginRatio d
    toSExpr (KicadThickness   d) = toSxD KeyThickness              d
    toSExpr (KicadWidth       d) = toSxD KeyWidth                  d
    toSExpr (KicadAngle       d) = toSxD KeyAngle                  d
    toSExpr (KicadSize      xy)  = toSxDD KeySize      xy
    toSExpr (KicadStart     xy)  = toSxDD KeyStart     xy
    toSExpr (KicadCenter    xy)  = toSxDD KeyCenter    xy
    toSExpr (KicadRectDelta xy)  = toSxDD KeyRectDelta xy
    toSExpr (KicadEnd       xy)  = toSxDD KeyEnd       xy
    toSExpr (KicadXy        xy)  = toSxDD KeyXy        xy
    toSExpr (KicadOffset    xy)  = toSxDD KeyOffset    xy
    toSExpr (KicadTedit s)       = toSxStr KeyTedit s
    toSExpr (KicadDescr s)       = toSxStr KeyDescr s
    toSExpr (KicadTags  s)       = toSxStr KeyTags  s
    toSExpr (KicadAttr  s)       = toSxStr KeyAttr  s
    toSExpr KicadItalic = AtomStr "italic"
    toSExpr KicadHide   = AtomStr "hide"

toSxD   kw d      = List [AtomKey kw, AtomDbl d]
toSxDD  kw (x,y)  = List [AtomKey kw, AtomDbl x, AtomDbl y]
toSxStr kw s      = List [AtomKey kw, AtomStr s]

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
    (KicadOffset    x) ~== (KicadOffset    y) = x ~== y
    (KicadClearance   x)       ~== (KicadClearance   y)       = x ~== y
    (KicadMaskMargin  x)       ~== (KicadMaskMargin  y)       = x ~== y
    (KicadPasteMargin x)       ~== (KicadPasteMargin y)       = x ~== y
    (KicadPasteMarginRatio  x) ~== (KicadPasteMarginRatio  y) = x ~== y
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

data KicadLayerT = FSilkS    | FCu       | FPaste    | FMask
                 | BSilkS    | BCu       | BPaste    | BMask
                 | DwgsUser  | CmtsUser  | FAdhes    | AllSilk
                 | FandBCu   | AllCu     | AllMask
                 | Inner1Cu  | Inner2Cu  | Inner3Cu  | Inner4Cu  | Inner5Cu
                 | Inner6Cu  | Inner7Cu  | Inner8Cu  | Inner9Cu  | Inner10Cu
                 | Inner11Cu | Inner12Cu | Inner13Cu | Inner14Cu | Inner15Cu
                 | Inner16Cu | Inner17Cu | Inner18Cu | Inner19Cu | Inner20Cu
                 | Inner21Cu | Inner22Cu | Inner23Cu | Inner24Cu | Inner25Cu
                 | Inner26Cu | Inner27Cu | Inner28Cu | Inner29Cu | Inner30Cu
                 | Inner31Cu | Inner32Cu
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
    , ("Cmts.User" , CmtsUser)
    , ("F.Adhes"   , FAdhes)
    , ("F&B.Cu"    , FandBCu)
    , ("*.Cu"      , AllCu  )
    , ("*.Mask"    , AllMask)
    , ("*.SilkS"   , AllSilk)
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
