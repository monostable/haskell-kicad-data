module Data.Kicad.Interpret
( interpret
)
where

import Data.Either
import Data.Maybe
import Data.Kicad.SExpr
import Data.Kicad.KicadExpr

interpret :: SExpr -> Either String KicadExpr
interpret (List (AtomKey kw:sxs)) =
    case go of
        Left err   -> Left $ "Could not interpret '" ++ show kw ++
                        "' because:\n\t" ++ err
        Right expr -> Right expr
    where go = case kw of
            KeyModule        -> fmap KicadExprModule    $ asKicadModule    sxs
            KeyFpLine        -> fmap KicadExprItem      $ asKicadFpLine    sxs
            KeyPad           -> fmap KicadExprItem      $ asKicadPad       sxs
            KeyFpText        -> fmap KicadExprItem      $ asKicadFpText    sxs
            KeyLayer         -> fmap KicadExprAttribute $ asKicadLayer     sxs
            KeyAt            -> fmap KicadExprAttribute $ asKicadAt        sxs
            KeyEffects       -> fmap KicadExprAttribute $ asKicadEffects   sxs
            KeyFont          -> fmap KicadExprAttribute $ asKicadFont      sxs
            KeySize          -> fmap KicadExprAttribute $ asKicadSize      sxs
            KeyThickness     -> fmap KicadExprAttribute $ asKicadThickness sxs
            KeyTEdit         -> fmap KicadExprAttribute $ asKicadTEdit     sxs
            KeyStart         -> fmap KicadExprAttribute $ asKicadStart     sxs
            KeyEnd           -> fmap KicadExprAttribute $ asKicadEnd       sxs
            KeyWidth         -> fmap KicadExprAttribute $ asKicadWidth     sxs
            KeyDescr         -> fmap KicadExprAttribute $ asKicadDescr     sxs
            KeyTags          -> fmap KicadExprAttribute $ asKicadTags      sxs
            KeyAttr          -> fmap KicadExprAttribute $ asKicadAttr      sxs
            KeyLayers        -> fmap KicadExprAttribute $ asKicadLayers    sxs
            KeyDrill         -> fmap KicadExprAttribute $ asKicadDrill     sxs
            KeyRectDelta     -> fmap KicadExprAttribute $ asKicadRectDelta sxs
            _ -> Left $ "-> Keyword '" ++ show kw
                    ++ "' is not implimented in interpreter"
interpret (AtomStr s) = case s of
    "italic" -> Right $ KicadExprAttribute KicadItalic
    "hide"   -> Right $ KicadExprAttribute KicadHide
    x -> expecting "'italic' or 'hide'" x
interpret x = expecting "List with a key or a string atom" x

asKicadAttribute :: [SExpr] -> Either String KicadAttribute
asKicadAttribute = undefined

asKicadModule :: [SExpr] -> Either String KicadModule
asKicadModule (AtomStr n:l@(List _):sxs) =
    case (interpret l) of
        Left err -> Left ('\t':err)
        Right (KicadExprAttribute (KicadLayer layer)) ->
            case (lefts expressions) of
                [] -> Right $ KicadModule
                    { kicadModuleName  = n
                    , kicadModuleLayer = layer
                    , kicadModuleItems = mapMaybe get_item $ rights expressions
                    }
                _  -> Left $ "Could not interpret expressions:\n"
                        ++ unlines (map ("\t\t"++) (lefts expressions))
            where expressions = map interpret sxs
                  get_item (KicadExprItem x) = Just x
                  get_item _ = Nothing
        _ -> expecting "layer (e.g. '(layer F.SilkS)')" $ List [l]
asKicadModule x = expecting "module name, layer and items " x

asKicadFpText :: [SExpr] -> Either String KicadItem
asKicadFpText (t:s:a:sxs) = interpretType
    where
        interpretType = case t of
            (AtomStr "reference") ->
                interpretString (defaultKicadFpText {fpTextType = FpTextReference})
            (AtomStr "value")     ->
                interpretString (defaultKicadFpText {fpTextType = FpTextValue})
            (AtomStr "user")     ->
                interpretString (defaultKicadFpText {fpTextType = FpTextUser})
            x           -> expecting "'reference', 'value' or 'user'" x
        interpretString fp_text = case s of
            (AtomStr string) -> interpretAt fp_text {fpTextStr = string}
            x           -> expecting "string" x
        interpretAt fp_text = case interpret a of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadAt at)) ->
                interpretRest sxs fp_text {fpTextAt = at}
            _ -> expecting "'at' expression (e.g. '(at 1.0 1.0)')" a
        interpretRest [] fp_text = Right fp_text
        interpretRest (sx:sxs) fp_text = case interpret sx of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadLayer layer)) ->
                interpretRest sxs (fp_text {fpTextLayer = layer})
            Right (KicadExprAttribute (KicadFpTextEffects
                    (KicadFont size thickness italic))) ->
                interpretRest sxs (fp_text { fpTextSize      = size
                                           , fpTextThickness = thickness
                                           , fpTextItalic    = italic
                                           }
                                  )
            Right (KicadExprAttribute KicadHide) ->
                interpretRest sxs (fp_text {fpTextHide = True})
            _ -> expecting "layer or effects expression or 'hide'" sx
asKicadFpText x = expecting "a text-type, text, 'at' and layer" $ List x

asKicadFpLine :: [SExpr] -> Either String KicadItem
asKicadFpLine (s:e:sxs) = interpretStart defaultKicadFpLine
    where
        interpretStart fp_line = case interpret s of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadStart start)) ->
                interpretEnd fp_line {fpLineStart = start}
        interpretEnd fp_line = case interpret e of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadEnd end)) ->
                interpretRest sxs fp_line {fpLineEnd = end}
        interpretRest [] fp_line = Right $ fp_line
        interpretRest (sx:sxs) fp_line = case interpret sx of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadWidth d)) -> interpretRest sxs fp_line {fpLineWidth = d}
            Right (KicadExprAttribute (KicadLayer d)) -> interpretRest sxs fp_line {fpLineLayer = d}

asKicadPad :: [SExpr] -> Either String KicadItem
asKicadPad (n:t:s:sxs) = interpretNumber
    where
        interpretNumber = case n of
            (AtomStr num) -> interpretType (defaultKicadPad {padNumber = num})
            x             -> expecting "string designating pad number" x
        interpretType :: KicadItem -> Either String KicadItem
        interpretType pad = case t of
            (AtomStr "smd")          ->
                interpretShape (pad {padType = SMD})
            (AtomStr "thru_hole")    ->
                interpretShape (pad {padType = ThruHole})
            (AtomStr "connect")      ->
                interpretShape (pad {padType = Connect})
            (AtomStr "np_thru_hole") ->
                interpretShape (pad {padType = NPThruHole})
            x -> expecting "pad type (e.g. 'smd')" x
        interpretShape :: KicadItem -> Either String KicadItem
        interpretShape pad = case s of
            (AtomStr "circle")    ->
                interpretRest sxs (pad {padShape = Circle})
            (AtomStr "oval")      ->
                interpretRest sxs (pad {padShape = Oval})
            (AtomStr "rect")      ->
                interpretRest sxs (pad {padShape = Rect})
            (AtomStr "trapezoid") ->
                interpretRest sxs (pad {padShape = Trapezoid})
            x -> expecting "pad shape (e.g. 'circle')" x
        interpretRest :: [SExpr] -> KicadItem -> Either String KicadItem
        interpretRest [] pad = Right pad
        interpretRest (sx:sxs) pad = case interpret sx of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadAt at) )->
                interpretRest sxs (pad {padAt = at})
            Right (KicadExprAttribute (KicadLayers layers)) ->
                interpretRest sxs (pad {padLayers = layers})
            Right (KicadExprAttribute  (KicadSize size)) ->
                interpretRest sxs (pad {padSize = size})
            Right (KicadExprAttribute (KicadDrill drill))     ->
                interpretRest sxs (pad {padDrill = Just drill})
            Right (KicadExprAttribute (KicadRectDelta delta)) ->
                interpretRest sxs (pad {padRectDelta = Just delta})
            _ -> expecting "at, size, drill, layers or nothing" $ sx
asKicadPad sxs = expecting "number, type and shape" $ List sxs


asKicadTEdit :: [SExpr] -> Either String KicadAttribute
asKicadTEdit [(AtomStr s)] = Right $ KicadTEdit s
asKicadTEdit x = expecting "timestamp only (String)" $ List x

asKicadLayer :: [SExpr] -> Either String KicadAttribute
asKicadLayer [sx] = oneKicadLayer sx
asKicadLayer x    = expecting "only one layer name" x

oneKicadLayer :: SExpr -> Either String KicadAttribute
oneKicadLayer (AtomStr n) = case n of
    "F.SilkS" -> Right (KicadLayer FSilkS)
    "F.Cu"    -> Right (KicadLayer FCu)
    "F.Paste" -> Right (KicadLayer FPaste)
    "F.Mask"  -> Right (KicadLayer FMask)
    "B.SilkS" -> Right (KicadLayer BSilkS)
    "B.Cu"    -> Right (KicadLayer BCu)
    "B.Paste" -> Right (KicadLayer BPaste)
    "B.Mask"  -> Right (KicadLayer BMask)
    "F&B.Cu"  -> Right (KicadLayer FandBCu)
    "*.Cu"    -> Right (KicadLayer AllCu)
    "*.Mask"  -> Right (KicadLayer AllMask)
    _         -> Left ("-> Unknown layer name: " ++ n)
oneKicadLayer x = expecting "layer name" x


asKicadAt :: [SExpr] -> Either String KicadAttribute
asKicadAt (AtomDbl x:[AtomDbl y]) =
    Right $ KicadAt $ defaultKicadAtT {kicadAtPoint = (x,y)}
asKicadAt (AtomDbl x:AtomDbl y:[AtomDbl o]) =
    Right $ KicadAt $ defaultKicadAtT
        { kicadAtPoint = (x,y)
        , kicadAtOrientation = o
        }
asKicadAt x =
    expecting "two floats (e.g. 1.0 1.0) and maybe an orientation (e.g. 90)"
    $ List x

asKicadEffects :: [SExpr] -> Either String KicadAttribute
asKicadEffects l@([e@(List _)]) =
    case (interpret e) of
        Left err -> Left ('\t':err)
        Right (KicadExprAttribute font@(KicadFont _ _ _))
            -> Right $ KicadFpTextEffects font
        _ -> expecting "font-expression" l
asKicadEffects x = expecting "one effects-expression (e.g. font)" $ List x

asKicadFont :: [SExpr] -> Either String KicadAttribute
asKicadFont sxs = interpretRest sxs defaultKicadFont
    where
        interpretRest [] font = Right font
        interpretRest (sx:sxs) font = case interpret sx of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadSize size)) ->
                interpretRest sxs font {kicadFontSize = size}
            Right (KicadExprAttribute (KicadThickness t)) ->
                interpretRest sxs font {kicadFontThickness = t}
            Right (KicadExprAttribute KicadItalic) ->
                interpretRest sxs font {kicadFontItalic = True}

asKicadSize :: [SExpr] -> Either String KicadAttribute
asKicadSize (AtomDbl x:[AtomDbl y]) = Right $ KicadSize (x, y)
asKicadSize x = expecting "two floats (e.g. '1.0 1.0')" x

asKicadThickness :: [SExpr] -> Either String KicadAttribute
asKicadThickness [AtomDbl thickness] = Right $ KicadThickness thickness
asKicadThickness x = expecting "one float only (e.g. '1.0')" x


asKicadStart [AtomDbl x, AtomDbl y] = Right $ KicadStart (x,y)
asKicadStart x = expecting "two floats (e.g. 1.0 1.0)" x

asKicadEnd [AtomDbl x, AtomDbl y] = Right $ KicadEnd (x,y)
asKicadEnd x = asKicadStart x

asKicadWidth [AtomDbl x] = Right $ KicadWidth x
asKicadWidth x = expecting "one float (e.g. 1.0)" x

asKicadDescr = asString KicadDescr
asKicadTags  = asString KicadTags
asKicadAttr  = asString KicadAttr

asString :: (String -> KicadAttribute) -> [SExpr] -> Either String KicadAttribute
asString kicad [AtomStr s] =  Right $ kicad s
asString _ x = expecting "string" x

asKicadLayers :: [SExpr] -> Either String KicadAttribute
asKicadLayers [] = Right $ KicadLayers []
asKicadLayers sxs = let layers = map oneKicadLayer sxs in case lefts layers of
    [] -> Right $ KicadLayers $ map (\(KicadLayer l) -> l) $ rights layers
    _  -> Left $ "Could not interpret layers:\n"
                    ++ unlines (map ("\t\t"++) (lefts layers))

asKicadDrill [(AtomDbl d)] = Right $ KicadDrill d
asKicadDrill x = expecting "one float (e.g. '1.0')" x

asKicadRectDelta [(AtomDbl y),(AtomDbl x)] --yes, y then x
    = Right $ KicadRectDelta (y,x)
asKicadRectDelta x = expecting "two floats (e.g '0 0.6')" x

expecting :: Show a => String -> a -> Either String b
expecting x y =
    Left $ "-> Expecting " ++ x ++ " but got " ++
        (nothing_or (strip_show y)) ++ " instead"
    where
        nothing_or y = case y of
            "" -> "nothing"
            y  -> "'" ++ y ++ "'"
        strip_show y = case (head (show y)) of
                '(' -> tail . init $ show y
                _   -> show y
