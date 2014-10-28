module Data.Kicad.Interpret
( interpret
)
where

import Data.Either
import Data.Kicad.SExpr
import Data.Kicad.KicadExpr
import Control.Applicative
import Lens.Family2 (over)

interpret :: SExpr -> Either String KicadExpr
interpret (List (AtomKey kw:sxs)) =
    case go of
        Left err   -> Left $ "Could not interpret '" ++ write kw ++
                        "' because:\n\t" ++ err
        Right expr -> Right expr
    where go = case kw of
            KeyModule    -> KicadExprModule    <$> asKicadModule           sxs
            KeyPad       -> KicadExprItem      <$> asKicadPad              sxs
            KeyFpText    -> KicadExprItem      <$> asKicadFpText           sxs
            KeyFpArc     -> KicadExprItem      <$> asKicadFpArc            sxs
            KeyFpPoly    -> KicadExprItem      <$> asKicadFpPoly           sxs
            KeyLayer     -> KicadExprAttribute <$> asKicadLayer            sxs
            KeyAt        -> KicadExprAttribute <$> asKicadAt               sxs
            KeyEffects   -> KicadExprAttribute <$> asKicadEffects          sxs
            KeyFont      -> KicadExprAttribute <$> asKicadFont             sxs
            KeyLayers    -> KicadExprAttribute <$> asKicadLayers           sxs
            KeyPts       -> KicadExprAttribute <$> asKicadPts              sxs
            KeyXyz       -> KicadExprAttribute <$> asKicadXyz              sxs
            KeyModel     -> KicadExprAttribute <$> asKicadModel            sxs
            KeyDrill     -> KicadExprAttribute <$> asKicadDrill            sxs
            KeySize      -> KicadExprAttribute <$> asXy KicadSize          sxs
            KeyStart     -> KicadExprAttribute <$> asXy KicadStart         sxs
            KeyEnd       -> KicadExprAttribute <$> asXy KicadEnd           sxs
            KeyCenter    -> KicadExprAttribute <$> asXy KicadCenter        sxs
            KeyRectDelta -> KicadExprAttribute <$> asXy KicadRectDelta     sxs
            KeyXy        -> KicadExprAttribute <$> asXy KicadXy            sxs
            KeyOffset    -> KicadExprAttribute <$> asXy KicadOffset        sxs
            KeyScale     -> KicadExprAttribute <$> asXyz KicadModelScale   sxs
            KeyRotate    -> KicadExprAttribute <$> asXyz KicadModelRotate  sxs
            KeyDescr     -> KicadExprAttribute <$> asString KicadDescr     sxs
            KeyTags      -> KicadExprAttribute <$> asString KicadTags      sxs
            KeyAttr      -> KicadExprAttribute <$> asString KicadAttr      sxs
            KeyTedit     -> KicadExprAttribute <$> asString KicadTedit     sxs
            KeyAngle     -> KicadExprAttribute <$> asDouble KicadAngle     sxs
            KeyThickness -> KicadExprAttribute <$> asDouble KicadThickness sxs
            KeyWidth     -> KicadExprAttribute <$> asDouble KicadWidth     sxs
            KeySolderPasteMarginRatio
                -> KicadExprAttribute <$> asDouble KicadPasteMarginRatio  sxs
            KeySolderPasteMargin
                -> KicadExprAttribute <$> asDouble KicadPasteMargin sxs
            KeySolderMaskMargin
                -> KicadExprAttribute <$> asDouble KicadMaskMargin  sxs
            KeyClearance
                -> KicadExprAttribute <$> asDouble KicadClearance   sxs
            KeyFpLine
                -> KicadExprItem <$> asFp defaultKicadFpLine        sxs
            KeyFpCircle
                -> KicadExprItem <$> asFp defaultKicadFpCircle      sxs
            KeyAutoplaceCost180
                -> KicadExprAttribute <$> asInt KicadAutoplaceCost180 sxs
            KeyAutoplaceCost90
                -> KicadExprAttribute <$> asInt KicadAutoplaceCost90 sxs
interpret (AtomStr s) = case s of
    "italic" -> Right $ KicadExprAttribute KicadItalic
    "hide"   -> Right $ KicadExprAttribute KicadHide
    "locked" -> Right $ KicadExprAttribute KicadLocked
    x -> expecting "'italic' or 'hide'" x
interpret x = expecting "List with a key or a string atom" x

asKicadModule :: [SExpr] -> Either String KicadModule
asKicadModule (AtomStr n:xs) =
    interpretRest xs defaultKicadModule { kicadModuleName = n }
    where
        interpretRest [] m = Right m
        interpretRest (sx:sxs) m = case interpret sx of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadLayer layer)) ->
                interpretRest sxs m {kicadModuleLayer = layer}
            Right (KicadExprItem item) ->
                interpretRest sxs (over moduleItems (item:) m)
            Right (KicadExprAttribute (KicadLocked)) -> interpretRest sxs m
            Right _ -> interpretRest sxs m
asKicadModule (x:_) = expecting "module name" x
asKicadModule x = expecting "module name" x

asKicadFpText :: [SExpr] -> Either String KicadItem
asKicadFpText (t:s:a:xs) = interpretType
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
                interpretRest xs fp_text {itemAt = at}
            _ -> expecting "'at' expression (e.g. '(at 1.0 1.0)')" a
        interpretRest [] fp_text = Right fp_text
        interpretRest (sx:sxs) fp_text = case interpret sx of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadLayer layer)) ->
                interpretRest sxs (fp_text {itemLayer = layer})
            Right (KicadExprAttribute (KicadFpTextEffects
                    (KicadFont size thickness italic))) ->
                interpretRest sxs (fp_text {  itemSize        = size
                                            , fpTextThickness = thickness
                                            , fpTextItalic    = italic
                                            }
                                   )
            Right (KicadExprAttribute KicadHide) ->
                interpretRest sxs (fp_text {fpTextHide = True})
            _ -> expecting "layer or effects expression or 'hide'" sx
asKicadFpText x = expecting "a text-type, text, 'at' and layer" x

asFp :: KicadItem -> [SExpr] -> Either String KicadItem
asFp defaultFp (s:e:xs) = interpretStart defaultFp
    where
        interpretStart fp_shape = case interpret s of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadStart start)) ->
                interpretEnd fp_shape {itemStart = start}
            Right (KicadExprAttribute (KicadCenter center)) ->
                interpretEnd fp_shape {itemStart = center}
            Right _ -> expecting "start (e.g. '(start 1.0 1.0)')" s
        interpretEnd fp_shape = case interpret e of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadEnd end)) ->
                interpretRest xs fp_shape {itemEnd = end}
            Right _ -> expecting "end (e.g. '(end 1.0 1.0)')" e
        interpretRest [] fp_shape = Right fp_shape
        interpretRest (sx:sxs) fp_shape = case interpret sx of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadWidth d))
                -> interpretRest sxs fp_shape {itemWidth = d}
            Right (KicadExprAttribute (KicadLayer d))
                -> interpretRest sxs fp_shape {itemLayer = d}
            Right _ -> expecting "width or layer" sx
asFp _ x = expecting "fp_line (or fp_circle) start (center), end and attributes" x

asKicadFpArc :: [SExpr] -> Either String KicadItem
asKicadFpArc (s:e:xs) = interpretStart defaultKicadFpArc
    where
        interpretStart fp_arc = case interpret s of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadStart start)) ->
                interpretEnd fp_arc {itemStart = start}
            Right _ -> expecting "start (e.g. '(start 1.0 1.0)')" s
        interpretEnd fp_arc = case interpret e of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadEnd end)) ->
                interpretRest xs fp_arc {itemEnd = end}
            Right _ -> expecting "end (e.g. '(end 1.0 1.0)')" e
        interpretRest [] fp_arc = Right fp_arc
        interpretRest (sx:sxs) fp_arc = case interpret sx of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadWidth d))
                -> interpretRest sxs fp_arc {itemWidth = d}
            Right (KicadExprAttribute (KicadLayer d))
                -> interpretRest sxs fp_arc {itemLayer = d}
            Right (KicadExprAttribute (KicadAngle d))
                -> interpretRest sxs fp_arc {fpArcAngle = d}
            Right _ -> expecting "width, layer or angle" sx
asKicadFpArc x = expecting "fp_arc start, end and attributes" x

asKicadFpPoly :: [SExpr] -> Either String KicadItem
asKicadFpPoly xs = interpretRest xs defaultKicadFpPoly
    where
        interpretRest [] fp_poly = Right fp_poly
        interpretRest (sx:sxs) fp_poly = case interpret sx of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadPts   d))
                -> interpretRest sxs fp_poly {fpPolyPts = d}
            Right (KicadExprAttribute (KicadWidth d))
                -> interpretRest sxs fp_poly {itemWidth = d}
            Right (KicadExprAttribute (KicadLayer d))
                -> interpretRest sxs fp_poly {itemLayer = d}
            Right _ -> expecting "width, layer or 'pts'" sx

asKicadPad :: [SExpr] -> Either String KicadItem
asKicadPad (n:t:s:xs) = interpretNumber
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
                interpretRest xs (pad {padShape = Circle})
            (AtomStr "oval")      ->
                interpretRest xs (pad {padShape = Oval})
            (AtomStr "rect")      ->
                interpretRest xs (pad {padShape = Rect})
            (AtomStr "trapezoid") ->
                interpretRest xs (pad {padShape = Trapezoid})
            x -> expecting "pad shape (e.g. 'circle')" x
        interpretRest :: [SExpr] -> KicadItem -> Either String KicadItem
        interpretRest [] pad = Right pad
        interpretRest (sx:sxs) pad = case interpret sx of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadAt d))
                -> interpretRest sxs pad {itemAt = d}
            Right (KicadExprAttribute (KicadLayers d))
                -> interpretRest sxs pad {padLayers = d}
            Right (KicadExprAttribute  (KicadSize d))
                -> interpretRest sxs pad {itemSize = d}
            Right (KicadExprAttribute a@(KicadDrill _))
                -> pushToAttrs sxs a pad
            Right (KicadExprAttribute a@(KicadRectDelta _))
                -> pushToAttrs sxs a pad
            Right (KicadExprAttribute a@(KicadMaskMargin _))
                -> pushToAttrs sxs a pad
            Right (KicadExprAttribute a@(KicadPasteMarginRatio _))
                -> pushToAttrs sxs a pad
            Right (KicadExprAttribute a@(KicadPasteMargin _))
                -> pushToAttrs sxs a pad
            Right (KicadExprAttribute a@(KicadClearance _))
                -> pushToAttrs sxs a pad
            _ -> expecting "at, size, drill, layers , margins or nothing" sx
        pushToAttrs sxs a pad = interpretRest sxs (over padAttributes (a:) pad)
asKicadPad xs = expecting "number, type and shape" $ List xs

asKicadLayer :: [SExpr] -> Either String KicadAttribute
asKicadLayer [sx] = oneKicadLayer sx
asKicadLayer x    = expecting "only one layer name" x

oneKicadLayer :: SExpr -> Either String KicadAttribute
oneKicadLayer (AtomStr n) = case strToLayer n of
    Just l  -> Right $ KicadLayer l
    Nothing -> Left ("-> Unknown layer name: " ++ n)
oneKicadLayer x = expecting "layer name" x

asKicadAt :: [SExpr] -> Either String KicadAttribute
asKicadAt (AtomDbl x:[AtomDbl y]) =
    Right $ KicadAt $ defaultKicadAtT {kicadAtPoint = (x,y)}
asKicadAt (AtomDbl x:AtomDbl y:[AtomDbl o]) =
    Right $ KicadAt $ KicadAtT (x,y) o
asKicadAt l@[List _] = asXyz KicadModelAt l
asKicadAt x =
    expecting "two or three floats or an 'xyz' expression"
    $ List x

asKicadEffects :: [SExpr] -> Either String KicadAttribute
asKicadEffects l@[e@(List _)] =
    case interpret e of
        Left err -> Left ('\t':err)
        Right (KicadExprAttribute font@(KicadFont {}))
            -> Right $ KicadFpTextEffects font
        _ -> expecting "font-expression" l
asKicadEffects x = expecting "one effects-expression (e.g. font)" x

asKicadFont :: [SExpr] -> Either String KicadAttribute
asKicadFont xs = interpretRest xs defaultKicadFont
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
            Right _ -> expecting "size, thickness or 'italic'" sx

asXy :: ((Double, Double) -> a) -> [SExpr] -> Either String a
asXy constructor [AtomDbl x, AtomDbl y] = Right $ constructor (x,y)
asXy _ x = expecting "two floats (e.g. 1.0 1.0)" x

asKicadPts :: [SExpr] -> Either String KicadAttribute
asKicadPts = fmap KicadPts . foldr interpretXys (Right [])
    where interpretXys sx z = case interpret sx of
                        Left err -> Left ('\t':err)
                        Right (KicadExprAttribute (KicadXy xy))
                            -> Right (xy:) <*> z
                        Right _ -> expecting "'xy' (e.g. '(xy 1.0 1.0)')" sx

asString :: (String -> KicadAttribute) -> [SExpr] -> Either String KicadAttribute
asString kicad [AtomStr s] =  Right $ kicad s
asString _ x = expecting "string" x

asKicadLayers :: [SExpr] -> Either String KicadAttribute
asKicadLayers [] = Right $ KicadLayers []
asKicadLayers xs = let layers = map oneKicadLayer xs in case lefts layers of
    [] -> Right $ KicadLayers $ map (\(KicadLayer l) -> l) $ rights layers
    _  -> Left $ "Could not interpret layers:\n"
                    ++ unlines (map ("\t\t"++) (lefts layers))

asDouble :: (Double -> KicadAttribute) -> [SExpr] -> Either String KicadAttribute
asDouble constructor [AtomDbl d] = Right $ constructor d
asDouble _ x = expecting "one float (e.g. '1.0')" x

asInt :: (Int -> KicadAttribute) -> [SExpr] -> Either String KicadAttribute
asInt constructor [AtomDbl d] = Right $ constructor $ round d
asInt _ x = expecting "one int (e.g. '1')" x

asKicadDrill :: [SExpr] -> Either String KicadAttribute
asKicadDrill xs = interpretRest xs defaultKicadDrillT
    where
        interpretRest [] drill = Right $ KicadDrill drill
        interpretRest (sx:sxs) drill = case sx of
            AtomDbl d  -> if kicadDrillSize drill == Nothing
                          then interpretRest sxs drill {kicadDrillSize = Just (d,d)}
                          else interpretRest sxs drill {
                                kicadDrillSize = fmap (\(x,_) -> (x,d)) (kicadDrillSize drill)
                                }
            AtomStr "oval"  -> interpretRest sxs drill {kicadDrillOval = True}
            (List _) -> case interpret sx of
                Left err -> Left ('\t':err)
                Right (KicadExprAttribute (KicadOffset xy))
                    -> interpretRest sxs drill {kicadDrillOffset = Just xy}
                Right _ -> expecting "offset or nothing" sx
            _ -> expecting "float, 'oval' or offset" sx

asKicadXyz :: [SExpr] -> Either String KicadAttribute
asKicadXyz (AtomDbl x:AtomDbl y:[AtomDbl z]) =
    Right $ KicadXyz (x,y,z)
asKicadXyz x = expecting "three floats" x

asXyz :: (KicadAttribute -> a) -> [SExpr] -> Either String a
asXyz constructor [l@(List _)] = case interpret l of
    Left err -> Left ('\t':err)
    Right (KicadExprAttribute xyz) -> Right $ constructor xyz
    Right _ -> expecting "xyz (e.g. '(xyz 1 1 1)')" l
asXyz _ x = expecting "xyz (e.g. '(xyz 1 1 1)')" x

asKicadModel :: [SExpr] -> Either String KicadAttribute
asKicadModel (AtomStr p:xs) = interpretRest xs defaultKicadModel {kicadModelPath = p}
    where
        interpretRest [] model = Right model
        interpretRest (sx:sxs) model = case interpret sx of
            Left err -> Left ('\t':err)
            Right (KicadExprAttribute (KicadModelAt (KicadXyz xyz))) ->
                interpretRest sxs model {kicadModelAt = xyz}
            Right (KicadExprAttribute (KicadModelScale (KicadXyz xyz))) ->
                interpretRest sxs model {kicadModelScale = xyz}
            Right (KicadExprAttribute (KicadModelRotate (KicadXyz xyz))) ->
                interpretRest sxs model {kicadModelRotate = xyz}
            Right _ -> expecting "only at, scale and rotate" sx
asKicadModel x = expecting "model path, at, scale and rotate" x

expecting :: Writable a => String -> a -> Either String b
expecting x y =
    Left $ "-> Expecting " ++ x ++ " but got " ++
        nothing_or (strip_brackets (write y)) ++ " instead"
    where
        nothing_or y' = case y' of
            "" -> "nothing"
            _  -> "'" ++ y' ++ "'"
        strip_brackets y' = case head y' of
                '(' -> tail . init $ y'
                _   -> y'
