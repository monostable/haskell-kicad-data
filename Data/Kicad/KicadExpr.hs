module Data.Kicad.KicadExpr where
import Lens.Family2

data KicadExpr = KicadExprModule KicadModule
               | KicadExprItem KicadItem
               | KicadExprAttribute KicadAttribute
    deriving (Show, Eq)

data KicadModule = KicadModule  { kicadModuleName  :: String
                                , kicadModuleLayer :: KicadLayerT
                                , kicadModuleItems :: [KicadItem]
                                }
    deriving (Show, Eq)

data KicadItem = KicadFpText { fpTextType      :: KicadFpTextTypeT
                             , fpTextStr       :: String
                             , fpTextAt        :: KicadAtT
                             , fpTextLayer     :: KicadLayerT
                             , fpTextHide      :: Bool
                             , fpTextSize      :: (Double, Double)
                             , fpTextThickness :: Double
                             , fpTextItalic    :: Bool
                             }
               | KicadFpLine { fpLineStart :: (Double, Double)
                             , fpLineEnd   :: (Double, Double)
                             , fpLineLayer :: KicadLayerT
                             , fpLineWidth :: Double
                             }
               | KicadPad { padNumber     :: String
                          , padType       :: KicadPadTypeT
                          , padShape      :: KicadPadShapeT
                          , padAt         :: KicadAtT
                          , padSize       :: (Double, Double)
                          , padLayers     :: [KicadLayerT]
                          , padDrill      :: Maybe Double
                          , padRectDelta  :: Maybe (Double, Double)
                          }
    deriving (Show, Eq)

layers :: Functor f => LensLike' f KicadItem [KicadLayerT]
layers f (KicadFpText t s a l h si th i) = (\(l':_) -> KicadFpText t s a l' h si th i) `fmap` (f [l])
layers f (KicadFpLine s e l w)           = (\(l':_) -> KicadFpLine s e l' w          ) `fmap` (f [l])
layers f (KicadPad n t s a si ls d r)    = (\ls'   -> KicadPad n t s a si ls' d r   ) `fmap` (f ls )

defaultKicadFpText = KicadFpText { fpTextType      = FpTextUser
                                 , fpTextStr       = ""
                                 , fpTextAt        = defaultKicadAtT
                                 , fpTextLayer     = FSilkS
                                 , fpTextHide      = False
                                 , fpTextSize      = (1.0, 1.0)
                                 , fpTextThickness = 1.0
                                 , fpTextItalic    = False
                                 }

defaultKicadFpLine = KicadFpLine { fpLineStart = (0,0)
                                 , fpLineEnd   = (0,0)
                                 , fpLineLayer = FSilkS
                                 , fpLineWidth = 0.15
                                 }

defaultKicadPad = KicadPad { padNumber     = ""
                           , padType       = ThruHole
                           , padShape      = Circle
                           , padAt         = defaultKicadAtT
                           , padSize       = (0,0)
                           , padLayers     = []
                           , padDrill      = Nothing
                           , padRectDelta  = Nothing
                           }

data KicadAttribute = KicadLayer KicadLayerT
                    | KicadAt KicadAtT
                    | KicadFpTextType KicadFpTextTypeT
                    | KicadSize (Double, Double)
                    | KicadThickness  Double
                    | KicadTEdit String
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
    deriving (Show, Eq)

defaultKicadFont = KicadFont { kicadFontSize = (1.0, 1.0)
                             , kicadFontThickness = 1.0
                             , kicadFontItalic = False
                             }

data KicadLayerT = FSilkS | FCu | FPaste | FMask
                 | BSilkS | BCu | BPaste | BMask
                 | FandBCu  | AllCu  | AllMask
    deriving (Show, Eq)

itemsOn :: KicadLayerT -> [KicadItem] -> [KicadItem]
itemsOn layer = filter ((layer `elem`) . view layers)

data KicadPadTypeT = ThruHole | SMD | Connect | NPThruHole
    deriving (Show, Eq)

data KicadPadShapeT = Circle | Oval | Rect | Trapezoid
    deriving (Show, Eq)

data KicadAtT = KicadAtT { kicadAtPoint :: (Double, Double)
                         , kicadAtOrientation :: Double
                         }
    deriving (Show, Eq)


defaultKicadAtT = KicadAtT { kicadAtPoint = (0,0)
                           , kicadAtOrientation = 0
                           }

data KicadFpTextTypeT = FpTextReference | FpTextValue | FpTextUser
    deriving (Show, Eq)
