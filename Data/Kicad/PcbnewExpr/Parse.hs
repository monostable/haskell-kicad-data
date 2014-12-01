module Data.Kicad.PcbnewExpr.Parse
( parse
, fromSExpr
)
where
import Data.Either
import Data.Maybe
import Control.Applicative
import Lens.Family2 (over)

import Data.Kicad.SExpr hiding (parse)
import qualified Data.Kicad.SExpr as SExpr (parse)
import Data.Kicad.PcbnewExpr.PcbnewExpr

{-| Parse a 'PcbnewExpr' from a 'String'. Returns an 'String' with an error or
   a 'PcbnewExpr'. -}
parse :: String -> Either String PcbnewExpr
parse = either Left fromSExpr . SExpr.parse

{-| Interpret a 'SExpr' as a 'PcbnewExpr'. -}
fromSExpr :: SExpr -> Either String PcbnewExpr
fromSExpr (List (AtomKey kw:sxs)) =
    case go of
        Left err   -> Left $ "Could not fromSExpr '" ++ writeKeyword kw ++
                        "' because:\n\t" ++ err
        Right expr -> Right expr
    where go = case kw of
            KeyModule    -> PcbnewExprModule    <$> asPcbnewModule           sxs
            KeyPad       -> PcbnewExprItem      <$> asPcbnewPad              sxs
            KeyFpText    -> PcbnewExprItem      <$> asPcbnewFpText           sxs
            KeyFpArc     -> PcbnewExprItem      <$> asPcbnewFpArc            sxs
            KeyFpPoly    -> PcbnewExprItem      <$> asPcbnewFpPoly           sxs
            KeyLayer     -> PcbnewExprAttribute <$> asPcbnewLayer            sxs
            KeyAt        -> PcbnewExprAttribute <$> asPcbnewAt               sxs
            KeyEffects   -> PcbnewExprAttribute <$> asPcbnewEffects          sxs
            KeyFont      -> PcbnewExprAttribute <$> asPcbnewFont             sxs
            KeyLayers    -> PcbnewExprAttribute <$> asPcbnewLayers           sxs
            KeyPts       -> PcbnewExprAttribute <$> asPcbnewPts              sxs
            KeyXyz       -> PcbnewExprAttribute <$> asPcbnewXyz              sxs
            KeyModel     -> PcbnewExprAttribute <$> asPcbnewModel            sxs
            KeyDrill     -> PcbnewExprAttribute <$> asPcbnewDrill            sxs
            KeySize      -> PcbnewExprAttribute <$> asXy PcbnewSize          sxs
            KeyStart     -> PcbnewExprAttribute <$> asXy PcbnewStart         sxs
            KeyEnd       -> PcbnewExprAttribute <$> asXy PcbnewEnd           sxs
            KeyCenter    -> PcbnewExprAttribute <$> asXy PcbnewCenter        sxs
            KeyRectDelta -> PcbnewExprAttribute <$> asXy PcbnewRectDelta     sxs
            KeyXy        -> PcbnewExprAttribute <$> asXy PcbnewXy            sxs
            KeyOffset    -> PcbnewExprAttribute <$> asXy PcbnewOffset        sxs
            KeyScale     -> PcbnewExprAttribute <$> asXyz PcbnewModelScale   sxs
            KeyRotate    -> PcbnewExprAttribute <$> asXyz PcbnewModelRotate  sxs
            KeyDescr     -> PcbnewExprAttribute <$> asString PcbnewDescr     sxs
            KeyTags      -> PcbnewExprAttribute <$> asString PcbnewTags      sxs
            KeyAttr      -> PcbnewExprAttribute <$> asString PcbnewAttr      sxs
            KeyTedit     -> PcbnewExprAttribute <$> asString PcbnewTedit     sxs
            KeyAngle     -> PcbnewExprAttribute <$> asDouble PcbnewAngle     sxs
            KeyThickness -> PcbnewExprAttribute <$> asDouble PcbnewThickness sxs
            KeyWidth     -> PcbnewExprAttribute <$> asDouble PcbnewWidth     sxs
            KeyThermalGap
                -> PcbnewExprAttribute <$> asDouble PcbnewThermalGap sxs
            KeyThermalWidth
                -> PcbnewExprAttribute <$> asDouble PcbnewThermalWidth sxs
            KeySolderPasteMarginRatio
                -> PcbnewExprAttribute <$> asDouble PcbnewPasteMarginRatio  sxs
            KeySolderPasteMargin
                -> PcbnewExprAttribute <$> asDouble PcbnewPasteMargin sxs
            KeySolderMaskMargin
                -> PcbnewExprAttribute <$> asDouble PcbnewMaskMargin  sxs
            KeyClearance
                -> PcbnewExprAttribute <$> asDouble PcbnewClearance   sxs
            KeyFpLine
                -> PcbnewExprItem <$> asFp defaultPcbnewFpLine        sxs
            KeyFpCircle
                -> PcbnewExprItem <$> asFp defaultPcbnewFpCircle      sxs
            KeyAutoplaceCost180
                -> PcbnewExprAttribute <$> asInt PcbnewAutoplaceCost180 sxs
            KeyAutoplaceCost90
                -> PcbnewExprAttribute <$> asInt PcbnewAutoplaceCost90 sxs
            KeyZoneConnect
                -> PcbnewExprAttribute <$> asInt PcbnewZoneConnect sxs
fromSExpr sx@(AtomStr s) = case s of
    "italic" -> Right $ PcbnewExprAttribute PcbnewItalic
    "hide"   -> Right $ PcbnewExprAttribute PcbnewHide
    "locked" -> Right $ PcbnewExprAttribute PcbnewLocked
    _ -> expecting "'italic' or 'hide' or 'locked' " sx
fromSExpr x = expecting "List with a key or a string atom" x

asPcbnewModule :: [SExpr] -> Either String PcbnewModule
asPcbnewModule (AtomStr n:xs) =
    interpretRest xs defaultPcbnewModule { pcbnewModuleName = n }
    where
        interpretRest [] m = Right m
        interpretRest (sx:sxs) m = case fromSExpr sx of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewLayer layer)) ->
                interpretRest sxs m {pcbnewModuleLayer = layer}
            Right (PcbnewExprItem item) ->
                interpretRest sxs (over moduleItems (++[item]) m)
            Right (PcbnewExprAttribute attr) ->
                interpretRest sxs (over moduleAttrs (++[attr]) m)
            Right _ -> expecting "layer, items or attributes" sx
asPcbnewModule (x:_) = expecting "module name" x
asPcbnewModule x = expecting' "module name" x

asPcbnewFpText :: [SExpr] -> Either String PcbnewItem
asPcbnewFpText (t:s:a:xs) = interpretType
    where
        interpretType = case t of
            (AtomStr "reference") ->
                interpretString (defaultPcbnewFpText {fpTextType = FpTextReference})
            (AtomStr "value")     ->
                interpretString (defaultPcbnewFpText {fpTextType = FpTextValue})
            (AtomStr "user")     ->
                interpretString (defaultPcbnewFpText {fpTextType = FpTextUser})
            _           -> expecting "'reference', 'value' or 'user'" t
        interpretString fp_text = case s of
            (AtomStr string) -> interpretAt fp_text {fpTextStr = string}
            _           -> expecting "string" s
        interpretAt fp_text = case fromSExpr a of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewAt at)) ->
                interpretRest xs fp_text {itemAt = at}
            _ -> expecting "'at' expression (e.g. '(at 1.0 1.0)')" a
        interpretRest [] fp_text = Right fp_text
        interpretRest (sx:sxs) fp_text = case fromSExpr sx of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewLayer layer)) ->
                interpretRest sxs (fp_text {itemLayer = layer})
            Right (PcbnewExprAttribute (PcbnewFpTextEffects
                    (PcbnewFont size thickness italic))) ->
                interpretRest sxs (fp_text {  itemSize        = size
                                            , fpTextThickness = thickness
                                            , fpTextItalic    = italic
                                            }
                                   )
            Right (PcbnewExprAttribute PcbnewHide) ->
                interpretRest sxs (fp_text {fpTextHide = True})
            _ -> expecting "layer or effects expression or 'hide'" sx
asPcbnewFpText x = expecting' "a text-type, text, 'at' and layer" x

asFp :: PcbnewItem -> [SExpr] -> Either String PcbnewItem
asFp defaultFp (s:e:xs) = interpretStart defaultFp
    where
        interpretStart fp_shape = case fromSExpr s of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewStart start)) ->
                interpretEnd fp_shape {itemStart = start}
            Right (PcbnewExprAttribute (PcbnewCenter center)) ->
                interpretEnd fp_shape {itemStart = center}
            Right _ -> expecting "start (e.g. '(start 1.0 1.0)')" s
        interpretEnd fp_shape = case fromSExpr e of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewEnd end)) ->
                interpretRest xs fp_shape {itemEnd = end}
            Right _ -> expecting "end (e.g. '(end 1.0 1.0)')" e
        interpretRest [] fp_shape = Right fp_shape
        interpretRest (sx:sxs) fp_shape = case fromSExpr sx of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewWidth d))
                -> interpretRest sxs fp_shape {itemWidth = d}
            Right (PcbnewExprAttribute (PcbnewLayer d))
                -> interpretRest sxs fp_shape {itemLayer = d}
            Right _ -> expecting "width or layer" sx
asFp _ x = expecting' "fp_line (or fp_circle) start (center), end and attributes" x

asPcbnewFpArc :: [SExpr] -> Either String PcbnewItem
asPcbnewFpArc (s:e:xs) = interpretStart defaultPcbnewFpArc
    where
        interpretStart fp_arc = case fromSExpr s of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewStart start)) ->
                interpretEnd fp_arc {itemStart = start}
            Right _ -> expecting "start (e.g. '(start 1.0 1.0)')" s
        interpretEnd fp_arc = case fromSExpr e of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewEnd end)) ->
                interpretRest xs fp_arc {itemEnd = end}
            Right _ -> expecting "end (e.g. '(end 1.0 1.0)')" e
        interpretRest [] fp_arc = Right fp_arc
        interpretRest (sx:sxs) fp_arc = case fromSExpr sx of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewWidth d))
                -> interpretRest sxs fp_arc {itemWidth = d}
            Right (PcbnewExprAttribute (PcbnewLayer d))
                -> interpretRest sxs fp_arc {itemLayer = d}
            Right (PcbnewExprAttribute (PcbnewAngle d))
                -> interpretRest sxs fp_arc {fpArcAngle = d}
            Right _ -> expecting "width, layer or angle" sx
asPcbnewFpArc x = expecting' "fp_arc start, end and attributes" x

asPcbnewFpPoly :: [SExpr] -> Either String PcbnewItem
asPcbnewFpPoly xs = interpretRest xs defaultPcbnewFpPoly
    where
        interpretRest [] fp_poly = Right fp_poly
        interpretRest (sx:sxs) fp_poly = case fromSExpr sx of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewPts   d))
                -> interpretRest sxs fp_poly {fpPolyPts = d}
            Right (PcbnewExprAttribute (PcbnewWidth d))
                -> interpretRest sxs fp_poly {itemWidth = d}
            Right (PcbnewExprAttribute (PcbnewLayer d))
                -> interpretRest sxs fp_poly {itemLayer = d}
            Right _ -> expecting "width, layer or 'pts'" sx

asPcbnewPad :: [SExpr] -> Either String PcbnewItem
asPcbnewPad (n:t:s:xs) = interpretNumber
    where
        interpretNumber = case n of
            (AtomStr num) -> interpretType defaultPcbnewPad {padNumber = num}
            _ -> expecting "string designating pad number" n
        interpretType :: PcbnewItem -> Either String PcbnewItem
        interpretType pad = case t of
            (AtomStr str) -> case strToPadType str of
                    Just d  -> interpretShape pad {padType = d}
                    Nothing ->
                        expecting "pad type (e.g. 'smd')" t
            _ -> expecting "pad type string (e.g. 'smd')" t
        interpretShape :: PcbnewItem -> Either String PcbnewItem
        interpretShape pad = case s of
            (AtomStr str) -> case strToPadShape str of
                    Just d  -> interpretRest xs pad {padShape = d}
                    Nothing ->
                        expecting "pad shape (e.g. 'circle')" s
            _ -> expecting "pad shape string (e.g. 'circle')" s
        interpretRest :: [SExpr] -> PcbnewItem -> Either String PcbnewItem
        interpretRest [] pad = Right pad
        interpretRest (sx:sxs) pad = case fromSExpr sx of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewAt d))
                -> interpretRest sxs pad {itemAt = d}
            Right (PcbnewExprAttribute (PcbnewLayers d))
                -> interpretRest sxs pad {padLayers = d}
            Right (PcbnewExprAttribute  (PcbnewSize d))
                -> interpretRest sxs pad {itemSize = d}
            Right (PcbnewExprAttribute a@(PcbnewDrill _))
                -> pushToAttrs sxs a pad
            Right (PcbnewExprAttribute a@(PcbnewRectDelta _))
                -> pushToAttrs sxs a pad
            Right (PcbnewExprAttribute a@(PcbnewMaskMargin _))
                -> pushToAttrs sxs a pad
            Right (PcbnewExprAttribute a@(PcbnewPasteMarginRatio _))
                -> pushToAttrs sxs a pad
            Right (PcbnewExprAttribute a@(PcbnewPasteMargin _))
                -> pushToAttrs sxs a pad
            Right (PcbnewExprAttribute a@(PcbnewClearance _))
                -> pushToAttrs sxs a pad
            Right (PcbnewExprAttribute a@(PcbnewZoneConnect _))
                -> pushToAttrs sxs a pad
            Right (PcbnewExprAttribute a@(PcbnewThermalWidth _))
                -> pushToAttrs sxs a pad
            Right (PcbnewExprAttribute a@(PcbnewThermalGap _))
                -> pushToAttrs sxs a pad
            _ -> expecting "at, size, drill, layers , margins etc. or nothing" sx
        pushToAttrs sxs a pad = interpretRest sxs (over padAttributes (++[a]) pad)
asPcbnewPad xs = expecting' "number, type and shape" xs

asPcbnewLayer :: [SExpr] -> Either String PcbnewAttribute
asPcbnewLayer [sx] = onePcbnewLayer sx
asPcbnewLayer x    = expecting' "only one layer name" x

onePcbnewLayer :: SExpr -> Either String PcbnewAttribute
onePcbnewLayer (AtomStr n) = case strToLayer n of
    Just l  -> Right $ PcbnewLayer l
    Nothing -> Left ("-> Unknown layer name: " ++ n)
onePcbnewLayer x = expecting "layer name" x

asPcbnewAt :: [SExpr] -> Either String PcbnewAttribute
asPcbnewAt (AtomDbl x:[AtomDbl y]) =
    Right $ PcbnewAt $ defaultPcbnewAtT {pcbnewAtPoint = (x,y)}
asPcbnewAt (AtomDbl x:AtomDbl y:[AtomDbl o]) =
    Right $ PcbnewAt $ PcbnewAtT (x,y) o
asPcbnewAt l@[List _] = asXyz PcbnewModelAt l
asPcbnewAt x =
    expecting' "two or three floats or an 'xyz' expression" x

asPcbnewEffects :: [SExpr] -> Either String PcbnewAttribute
asPcbnewEffects [e@(List _)] =
    case fromSExpr e of
        Left err -> Left ('\t':err)
        Right (PcbnewExprAttribute font@(PcbnewFont {}))
            -> Right $ PcbnewFpTextEffects font
        _ -> expecting "font-expression" e
asPcbnewEffects x = expecting' "one effects-expression (e.g. font)" x

asPcbnewFont :: [SExpr] -> Either String PcbnewAttribute
asPcbnewFont xs = interpretRest xs defaultPcbnewFont
    where
        interpretRest [] font = Right font
        interpretRest (sx:sxs) font = case fromSExpr sx of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewSize size)) ->
                interpretRest sxs font {pcbnewFontSize = size}
            Right (PcbnewExprAttribute (PcbnewThickness t)) ->
                interpretRest sxs font {pcbnewFontThickness = t}
            Right (PcbnewExprAttribute PcbnewItalic) ->
                interpretRest sxs font {pcbnewFontItalic = True}
            Right _ -> expecting "size, thickness or 'italic'" sx

asXy :: ((Double, Double) -> a) -> [SExpr] -> Either String a
asXy constructor [AtomDbl x, AtomDbl y] = Right $ constructor (x,y)
asXy _ x = expecting' "two floats (e.g. 1.0 1.0)" x

asPcbnewPts :: [SExpr] -> Either String PcbnewAttribute
asPcbnewPts = fmap PcbnewPts . foldr interpretXys (Right [])
    where interpretXys sx z = case fromSExpr sx of
                        Left err -> Left ('\t':err)
                        Right (PcbnewExprAttribute (PcbnewXy xy))
                            -> Right (xy:) <*> z
                        Right _ -> expecting "'xy' (e.g. '(xy 1.0 1.0)')" sx

asString :: (String -> PcbnewAttribute) -> [SExpr] -> Either String PcbnewAttribute
asString pcbnew [AtomStr s] =  Right $ pcbnew s
asString _ x = expecting' "string" x

asPcbnewLayers :: [SExpr] -> Either String PcbnewAttribute
asPcbnewLayers [] = Right $ PcbnewLayers []
asPcbnewLayers xs = let layers = map onePcbnewLayer xs in case lefts layers of
    [] -> Right $ PcbnewLayers $ map (\(PcbnewLayer l) -> l) $ rights layers
    _  -> Left $ "Could not fromSExpr layers:\n"
                    ++ unlines (map ("\t\t"++) (lefts layers))

asDouble :: (Double -> PcbnewAttribute) -> [SExpr] -> Either String PcbnewAttribute
asDouble constructor [AtomDbl d] = Right $ constructor d
asDouble _ x = expecting' "one float (e.g. '1.0')" x

asInt :: (Int -> PcbnewAttribute) -> [SExpr] -> Either String PcbnewAttribute
asInt constructor [AtomDbl d] = Right $ constructor $ round d
asInt _ x = expecting' "one int (e.g. '1')" x

asPcbnewDrill :: [SExpr] -> Either String PcbnewAttribute
asPcbnewDrill xs = interpretRest xs defaultPcbnewDrillT
    where
        interpretRest [] drill = Right $ PcbnewDrill drill
        interpretRest (sx:sxs) drill = case sx of
            AtomDbl d  -> if isNothing (pcbnewDrillSize drill)
                          then interpretRest sxs drill
                                { pcbnewDrillSize = Just (d,d) }
                          else interpretRest sxs drill
                               { pcbnewDrillSize =
                                    fmap (\(x,_) -> (x,d)) (pcbnewDrillSize drill)
                               }
            AtomStr "oval"  -> interpretRest sxs drill {pcbnewDrillOval = True}
            (List _) -> case fromSExpr sx of
                Left err -> Left ('\t':err)
                Right (PcbnewExprAttribute (PcbnewOffset xy))
                    -> interpretRest sxs drill {pcbnewDrillOffset = Just xy}
                Right _ -> expecting "offset or nothing" sx
            _ -> expecting "float, 'oval' or offset" sx

asPcbnewXyz :: [SExpr] -> Either String PcbnewAttribute
asPcbnewXyz (AtomDbl x:AtomDbl y:[AtomDbl z]) =
    Right $ PcbnewXyz (x,y,z)
asPcbnewXyz x = expecting' "three floats" x

asXyz :: (PcbnewAttribute -> a) -> [SExpr] -> Either String a
asXyz constructor [l@(List _)] = case fromSExpr l of
    Left err -> Left ('\t':err)
    Right (PcbnewExprAttribute xyz) -> Right $ constructor xyz
    Right _ -> expecting "xyz (e.g. '(xyz 1 1 1)')" l
asXyz _ x = expecting' "xyz (e.g. '(xyz 1 1 1)')" x

asPcbnewModel :: [SExpr] -> Either String PcbnewAttribute
asPcbnewModel (AtomStr p:xs) = interpretRest xs defaultPcbnewModel {pcbnewModelPath = p}
    where
        interpretRest [] model = Right model
        interpretRest (sx:sxs) model = case fromSExpr sx of
            Left err -> Left ('\t':err)
            Right (PcbnewExprAttribute (PcbnewModelAt (PcbnewXyz xyz))) ->
                interpretRest sxs model {pcbnewModelAt = xyz}
            Right (PcbnewExprAttribute (PcbnewModelScale (PcbnewXyz xyz))) ->
                interpretRest sxs model {pcbnewModelScale = xyz}
            Right (PcbnewExprAttribute (PcbnewModelRotate (PcbnewXyz xyz))) ->
                interpretRest sxs model {pcbnewModelRotate = xyz}
            Right _ -> expecting "only at, scale and rotate" sx
asPcbnewModel x = expecting' "model path, at, scale and rotate" x

expecting :: String -> SExpr -> Either String a
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

expecting' :: String -> [SExpr] -> Either String a
expecting' x y = expecting x $ List y
