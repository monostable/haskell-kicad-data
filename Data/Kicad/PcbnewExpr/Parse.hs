module Data.Kicad.PcbnewExpr.Parse
( parse
, parseWithFilename
, fromSExpr
)
where
import Data.Either
import Data.Maybe
import Lens.Family2 (over)
import Data.List (intersperse)
import Text.Read (readMaybe)
import Text.Parsec.Pos (newPos)
import Control.Applicative ((<$>), (<*>))

import Data.Kicad.SExpr hiding (parse, parseWithFilename)
import qualified Data.Kicad.SExpr as SExpr (parseWithFilename)
import Data.Kicad.PcbnewExpr.PcbnewExpr
import Data.Kicad.Util (headOr)

{-| Parse a Pcbnew expression from a string. Returns an 'String' with an error
 - or a 'PcbnewExpr'. -}
parse :: String -> Either String PcbnewExpr
parse = parseWithFilename ""

{-| Parse a Pcbnew expression from a string giving a filename argument to be used in error strings. -}
parseWithFilename :: String -> String -> Either String PcbnewExpr
parseWithFilename filename =
    either Left fromSExpr . SExpr.parseWithFilename filename

{-| Interpret a 'SExpr' as a 'PcbnewExpr'. -}
fromSExpr :: SExpr -> Either String PcbnewExpr
fromSExpr (List _ (Atom pos kw:sxs)) = case kw of
    "footprint"  -> PcbnewExprFootprint <$> asPcbnewFootprint        sxs
    "module"     -> PcbnewExprModule    <$> asPcbnewModule           sxs
    "pad"        -> PcbnewExprItem      <$> asPcbnewPad              sxs
    "fp_text"    -> PcbnewExprItem      <$> asPcbnewFpText           sxs
    "fp_arc"     -> PcbnewExprItem      <$> asPcbnewFpArc            sxs
    "fp_poly"    -> PcbnewExprItem      <$> asPcbnewFpPoly           sxs
    "group"      -> PcbnewExprItem      <$> asPcbnewGroup            sxs
    "layer"      -> PcbnewExprAttribute <$> asPcbnewLayer            sxs
    "at"         -> PcbnewExprAttribute <$> asPcbnewAt               sxs
    "effects"    -> PcbnewExprAttribute <$> asPcbnewEffects          sxs
    "font"       -> PcbnewExprAttribute <$> asPcbnewFont             sxs
    "layers"     -> PcbnewExprAttribute <$> asPcbnewLayers           sxs
    "pts"        -> PcbnewExprAttribute <$> asPcbnewPts              sxs
    "xyz"        -> PcbnewExprAttribute <$> asPcbnewXyz              sxs
    "model"      -> PcbnewExprAttribute <$> asPcbnewModel            sxs
    "drill"      -> PcbnewExprAttribute <$> asPcbnewDrill            sxs
    "size"       -> PcbnewExprAttribute <$> asXy PcbnewSize          sxs
    "start"      -> PcbnewExprAttribute <$> asXy PcbnewStart         sxs
    "end"        -> PcbnewExprAttribute <$> asXy PcbnewEnd           sxs
    "center"     -> PcbnewExprAttribute <$> asXy PcbnewCenter        sxs
    "rect_delta" -> PcbnewExprAttribute <$> asXy PcbnewRectDelta     sxs
    "xy"         -> PcbnewExprAttribute <$> asXy PcbnewXy            sxs
    "scale"      -> PcbnewExprAttribute <$> asXyz PcbnewModelScale   sxs
    "rotate"     -> PcbnewExprAttribute <$> asXyz PcbnewModelRotate  sxs
    "version"    -> PcbnewExprAttribute <$> asString PcbnewVersion   sxs
    "tstamp"     -> PcbnewExprAttribute <$> asString PcbnewTstamp    sxs
    "generator"  -> PcbnewExprAttribute <$> asString PcbnewGenerator sxs
    "descr"      -> PcbnewExprAttribute <$> asString PcbnewDescr     sxs
    "tags"       -> PcbnewExprAttribute <$> asString PcbnewTags      sxs
    "path"       -> PcbnewExprAttribute <$> asString PcbnewPath      sxs
    "attr"       -> PcbnewExprAttribute <$> asString PcbnewAttr      sxs
    "tedit"      -> PcbnewExprAttribute <$> asString PcbnewTedit     sxs
    "id"         -> PcbnewExprAttribute <$> asString PcbnewId        sxs
    "angle"      -> PcbnewExprAttribute <$> asDouble PcbnewAngle     sxs
    "thickness"  -> PcbnewExprAttribute <$> asDouble PcbnewThickness sxs
    "width"      -> PcbnewExprAttribute <$> asDouble PcbnewWidth     sxs
    "justify"    -> PcbnewExprAttribute <$> asPcbnewJustifyT         sxs
    "zone_connect"
        -> PcbnewExprAttribute <$> asZoneConnect sxs
    "thermal_gap"
        -> PcbnewExprAttribute <$> asDouble PcbnewThermalGap sxs
    "thermal_width"
        -> PcbnewExprAttribute <$> asDouble PcbnewThermalWidth sxs
    "solder_paste_margin_ratio"
        -> PcbnewExprAttribute <$> asDouble PcbnewPasteMarginRatio sxs
    "solder_paste_margin"
        -> PcbnewExprAttribute <$> asDouble PcbnewPasteMargin sxs
    "solder_mask_margin"
        -> PcbnewExprAttribute <$> asDouble PcbnewMaskMargin sxs
    "solder_paste_ratio"
        -> PcbnewExprAttribute <$> asDouble PcbnewSolderPasteRatio sxs
    "fp_line"
        -> PcbnewExprItem <$> asFp defaultPcbnewFpLine sxs
    "fp_circle"
        -> PcbnewExprItem <$> asFp defaultPcbnewFpCircle sxs
    "fp_rect"
        -> PcbnewExprItem <$> asFp defaultPcbnewFpRect sxs
    "autoplace_cost180"
        -> PcbnewExprAttribute <$> asInt PcbnewAutoplaceCost180 sxs
    "autoplace_cost90"
        -> PcbnewExprAttribute <$> asInt PcbnewAutoplaceCost90 sxs
    "roundrect_rratio"
        -> PcbnewExprAttribute <$> asDouble PcbnewRoundrectRratio sxs
    "die_length"
        -> PcbnewExprAttribute <$> asDouble PcbnewDieLength sxs
    "offset" -> PcbnewExprAttribute <$> case (asXy PcbnewOffset sxs) of
                                           Right _ -> asXy PcbnewOffset sxs
                                           Left _ -> asXyz PcbnewModelOffset sxs
    "fill" -> PcbnewExprAttribute <$> case sxs of
                  [Atom _ "none"] -> Right $ PcbnewShapeFill False
                  [Atom _ "solid"]-> Right $ PcbnewShapeFill True
                  [Atom _ "yes"]  -> Right $ PcbnewGrItemFill
                  _ -> expecting' "'none' or 'solid'" sxs
    "clearance" -> PcbnewExprAttribute <$> case asDouble PcbnewClearance sxs of
                                             Right x -> Right x
                                             Left err -> asPcbnewOptionsClearance sxs
    "locked" -> Right $ PcbnewExprAttribute PcbnewLocked
    "members" -> PcbnewExprAttribute <$> asStrings PcbnewMembers sxs
    "options" -> PcbnewExprAttribute <$> asPcbnewOptions sxs
    "anchor" -> PcbnewExprAttribute <$> asPcbnewOptionsAnchor sxs
    "primitives" -> PcbnewExprAttribute <$> asPcbnewPrimitives sxs
    _ -> Left $ "Error in " ++ (show pos) ++ ": unknown expression type '" ++ kw ++ "'"

fromSExpr sx@(Atom _ s) = case s of
    "italic" -> Right $ PcbnewExprAttribute PcbnewItalic
    "hide"   -> Right $ PcbnewExprAttribute PcbnewHide
    "locked" -> Right $ PcbnewExprAttribute PcbnewLocked
    "placed" -> Right $ PcbnewExprAttribute PcbnewPlaced
    _ -> expecting "'italic' or 'hide' or 'locked' " sx
fromSExpr x = expecting "List _ with a key or a string atom" x

asPcbnewModule :: [SExpr] -> Either String PcbnewModule
asPcbnewModule (Atom _ n:xs) =
    interpretRest xs defaultPcbnewModule { pcbnewModuleName = n }
    where
        interpretRest [] m = Right m
        interpretRest (sx:sxs) m = case fromSExpr sx of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewLayer layer)) ->
                interpretRest sxs m {pcbnewModuleLayer = layer}
            Right (PcbnewExprItem item) ->
                interpretRest sxs (over moduleItems (++[item]) m)
            Right (PcbnewExprAttribute attr) ->
                interpretRest sxs (over moduleAttrs (++[attr]) m)
            Right _ -> expecting "layer, items or attributes" sx
asPcbnewModule (x:_) = expecting "module name" x
asPcbnewModule x = expecting' "module name" x

asPcbnewFootprint :: [SExpr] -> Either String PcbnewFootprint
asPcbnewFootprint (Atom _ n:xs) =
    interpretRest xs defaultPcbnewFootprint { pcbnewFootprintName = n }
    where
        interpretRest [] m = Right m
        interpretRest (sx:sxs) m = case fromSExpr sx of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewLayer layer)) ->
                interpretRest sxs m {pcbnewFootprintLayer = layer}
            Right (PcbnewExprAttribute (PcbnewVersion version)) ->
                interpretRest sxs m {pcbnewFootprintVersion = version}
            Right (PcbnewExprAttribute (PcbnewGenerator generator)) ->
                interpretRest sxs m {pcbnewFootprintGenerator = generator}
            Right (PcbnewExprItem item) ->
                interpretRest sxs (over footprintItems (++[item]) m)
            Right (PcbnewExprAttribute attr) ->
                interpretRest sxs (over footprintAttrs (++[attr]) m)
            Right _ -> expecting "layer, items or attributes" sx
asPcbnewFootprint (x:_) = expecting "footprint name" x
asPcbnewFootprint x = expecting' "footprint name" x

asPcbnewFpText :: [SExpr] -> Either String PcbnewItem
asPcbnewFpText (t:s:a:xs) = interpretType
    where
        interpretType = case t of
            (Atom _ "reference") ->
                interpretString (defaultPcbnewFpText {fpTextType = FpTextReference})
            (Atom _ "value")     ->
                interpretString (defaultPcbnewFpText {fpTextType = FpTextValue})
            (Atom _ "user")     ->
                interpretString (defaultPcbnewFpText {fpTextType = FpTextUser})
            _           -> expecting "'reference', 'value' or 'user'" t
        interpretString fp_text = case s of
            (Atom _ string) -> interpretAt fp_text {fpTextStr = string}
            _           -> expecting "string" s
        interpretAt fp_text = case fromSExpr a of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewAt at)) ->
                interpretRest xs fp_text {itemAt = at}
            _ -> expecting "'at' expression (e.g. '(at 1.0 1.0)')" a
        interpretEffects [] fp_text = fp_text
        interpretEffects (e:efs) fp_text = case e of
            (PcbnewJustify js) ->
               interpretEffects efs (over fpTextJustify (++ js) fp_text)
            (PcbnewFont size thickness italic) ->
               interpretEffects efs
                   (fp_text
                       { itemSize = size
                       , fpTextThickness = thickness
                       , fpTextItalic    = italic
                       }
                   )
            _ -> fp_text
        interpretRest [] fp_text = Right fp_text
        interpretRest (sx:sxs) fp_text = case fromSExpr sx of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewLayer layer)) ->
                interpretRest sxs (fp_text {itemLayer = layer})
            Right (PcbnewExprAttribute (PcbnewFpTextEffects effects)) ->
                interpretRest sxs (interpretEffects effects fp_text)
            Right (PcbnewExprAttribute PcbnewHide) ->
                interpretRest sxs (fp_text {fpTextHide = True})
            Right (PcbnewExprAttribute (PcbnewTstamp uuid)) ->
               interpretRest sxs fp_text {itemTstamp = uuid}
            _ -> expecting "layer or effects expression or 'hide'" sx
asPcbnewFpText x = expecting' "a text-type, text, 'at' and layer" x

asFp :: PcbnewItem -> [SExpr] -> Either String PcbnewItem
asFp defaultFp (s:e:xs) = interpretStart defaultFp
    where
        interpretStart fp_shape = case fromSExpr s of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewStart start)) ->
                interpretEnd fp_shape {itemStart = start}
            Right (PcbnewExprAttribute (PcbnewCenter center)) ->
                interpretEnd fp_shape {itemStart = center}
            Right _ -> expecting "start (e.g. '(start 1.0 1.0)')" s
        interpretEnd fp_shape = case fromSExpr e of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewEnd end)) ->
                interpretRest xs fp_shape {itemEnd = end}
            Right _ -> expecting "end (e.g. '(end 1.0 1.0)')" e
        interpretRest [] fp_shape = Right fp_shape
        interpretRest (sx:sxs) fp_shape = case fromSExpr sx of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewWidth d))
                -> interpretRest sxs $ fp_shape {itemWidth = d}
            Right (PcbnewExprAttribute (PcbnewLayer d))
                -> interpretRest sxs $ fp_shape {itemLayer = d}
            Right (PcbnewExprAttribute (PcbnewShapeFill b))

                -> interpretRest sxs $ fp_shape {itemFill = Just b}
            Right (PcbnewExprAttribute (PcbnewTstamp uuid)) ->
               interpretRest sxs $ fp_shape {itemTstamp = uuid}
            Right _ -> expecting "width or layer" sx
asFp _ x = expecting' "fp_line (or fp_circle) start (center), end and attributes" x

asPcbnewFpArc :: [SExpr] -> Either String PcbnewItem
asPcbnewFpArc (s:e:xs) = interpretStart defaultPcbnewFpArc
    where
        interpretStart fp_arc = case fromSExpr s of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewStart start)) ->
                interpretEnd fp_arc {itemStart = start}
            Right _ -> expecting "start (e.g. '(start 1.0 1.0)')" s
        interpretEnd fp_arc = case fromSExpr e of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewEnd end)) ->
                interpretRest xs fp_arc {itemEnd = end}
            Right _ -> expecting "end (e.g. '(end 1.0 1.0)')" e
        interpretRest [] fp_arc = Right fp_arc
        interpretRest (sx:sxs) fp_arc = case fromSExpr sx of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewWidth d))
                -> interpretRest sxs fp_arc {itemWidth = d}
            Right (PcbnewExprAttribute (PcbnewLayer d))
                -> interpretRest sxs fp_arc {itemLayer = d}
            Right (PcbnewExprAttribute (PcbnewAngle d))
                -> interpretRest sxs fp_arc {fpArcAngle = d}
            Right (PcbnewExprAttribute (PcbnewTstamp uuid)) ->
               interpretRest sxs fp_arc {itemTstamp = uuid}
            Right _ -> expecting "width, layer or angle" sx
asPcbnewFpArc x = expecting' "fp_arc start, end and attributes" x

asPcbnewFpPoly :: [SExpr] -> Either String PcbnewItem
asPcbnewFpPoly xs = interpretRest xs defaultPcbnewFpPoly
    where
        interpretRest [] fp_poly = Right fp_poly
        interpretRest (sx:sxs) fp_poly = case fromSExpr sx of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewPts   d))
                -> interpretRest sxs fp_poly {fpPolyPts = d}
            Right (PcbnewExprAttribute (PcbnewWidth d))
                -> interpretRest sxs fp_poly {itemWidth = d}
            Right (PcbnewExprAttribute (PcbnewLayer d))
                -> interpretRest sxs fp_poly {itemLayer = d}
            Right (PcbnewExprAttribute (PcbnewTstamp uuid)) ->
               interpretRest sxs fp_poly {itemTstamp = uuid}
            Right _ -> expecting "width, layer or 'pts'" sx

asPcbnewGrPoly :: [SExpr] -> Either String PcbnewGrItem
asPcbnewGrPoly xs = undefined

asPcbnewGroup :: [SExpr] -> Either String PcbnewItem
asPcbnewGroup (Atom _ n:xs) =
    interpretRest xs defaultPcbnewGroup { groupName = n }
    where
        interpretRest [] m = Right m
        interpretRest (sx:sxs) m = case fromSExpr sx of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewId i)) ->
                interpretRest sxs m {groupId = i}
            Right (PcbnewExprAttribute (PcbnewMembers ms)) ->
                interpretRest sxs m {groupMembers = ms}
            Right _ -> expecting "id or member" sx
asPcbnewGroup (x:_) = expecting "group name" x
asPcbnewGroup x = expecting' "group name" x

asPcbnewPad :: [SExpr] -> Either String PcbnewItem
asPcbnewPad (n:t:s:xs) = interpretNumber
    where
        interpretNumber = case n of
            (Atom _ num) -> interpretType defaultPcbnewPad {padNumber = num}
            _ -> expecting "string designating pad number" n
        interpretType :: PcbnewItem -> Either String PcbnewItem
        interpretType pad = case t of
            (Atom _ str) -> case strToPadType str of
                    Just d  -> interpretShape pad {padType = d}
                    Nothing ->
                        expecting "pad type (e.g. 'smd')" t
            _ -> expecting "pad type string (e.g. 'smd')" t
        interpretShape :: PcbnewItem -> Either String PcbnewItem
        interpretShape pad = case s of
            (Atom _ str) -> case strToPadShape str of
                    Just d  -> interpretRest xs pad {padShape = d}
                    Nothing ->
                        expecting "pad shape (e.g. 'circle')" s
            _ -> expecting "pad shape string (e.g. 'circle')" s
        interpretRest :: [SExpr] -> PcbnewItem -> Either String PcbnewItem
        interpretRest [] pad = Right pad
        interpretRest (sx:sxs) pad = case fromSExpr sx of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewAt d))
                -> interpretRest sxs pad {itemAt = d}
            Right (PcbnewExprAttribute (PcbnewLayers d))
                -> interpretRest sxs pad {padLayers = d}
            Right (PcbnewExprAttribute  (PcbnewSize d))
                -> interpretRest sxs pad {itemSize = d}
            Right (PcbnewExprAttribute (PcbnewTstamp uuid)) ->
               interpretRest sxs pad {itemTstamp = uuid}
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
            Right (PcbnewExprAttribute a@(PcbnewRoundrectRratio _))
                -> pushToAttrs sxs a pad
            Right (PcbnewExprAttribute a@(PcbnewDieLength _))
                -> pushToAttrs sxs a pad
            Right (PcbnewExprAttribute a@(PcbnewLocked))
                -> pushToAttrs sxs a pad
            Right (PcbnewExprAttribute a@(PcbnewOptions _ _))
                -> pushToAttrs sxs a pad
            Right (PcbnewExprAttribute a@(PcbnewPrimitives _))
                -> pushToAttrs sxs a pad
            _ -> expecting "at, size, drill, layers , margins etc. or nothing" sx
        pushToAttrs sxs a pad = interpretRest sxs (over padAttributes (++[a]) pad)
asPcbnewPad xs = expecting' "number, type and shape" xs

asPcbnewLayer :: [SExpr] -> Either String PcbnewAttribute
asPcbnewLayer [sx] = onePcbnewLayer sx
asPcbnewLayer x    = expecting' "only one layer name" x

onePcbnewLayer :: SExpr -> Either String PcbnewAttribute
onePcbnewLayer (Atom _ n) = case strToLayer n of
    Just l  -> Right $ PcbnewLayer l
    Nothing -> Left ("-> Unknown layer name: " ++ n)
onePcbnewLayer x = expecting "layer name" x

asZoneConnect :: [SExpr] -> Either String PcbnewAttribute
asZoneConnect [Atom _ n] = case strToZoneConnect n of
    Just zc -> Right $ PcbnewZoneConnect zc
    Nothing -> Left $ "-> Unknown zone_connect " ++ n
asZoneConnect x = expecting' "one number" x

asPcbnewAt :: [SExpr] -> Either String PcbnewAttribute
asPcbnewAt sx@(Atom _ x:[Atom _ y]) = case readXy x y of
    Just xy -> Right $ PcbnewAt $ defaultPcbnewAtT {pcbnewAtPoint = xy}
    Nothing -> expecting' "x y coordinates" sx
asPcbnewAt sx@(Atom posx x:Atom posy y:[Atom _ o]) =
   case o of
     "unlocked" -> case readXy x y of
       Just xy -> Right $ PcbnewAt $ defaultPcbnewAtT {pcbnewAtPoint = xy, pcbnewAtUnlocked = True}
       Nothing -> expecting' "x y coordinates" sx
     _ -> case readXyz x y o of
       Just (x', y', o') -> Right $ PcbnewAt $ PcbnewAtT (x',y') o' False
       Nothing -> expecting' "x y coordinates and orientation" sx
asPcbnewAt l@[List _ _] = asXyz PcbnewModelAt l
asPcbnewAt x =
    expecting' "x y coordinates and orientation" x

readXy :: String -> String -> Maybe (Double, Double)
readXy x y = do
   x' <- readMaybeDouble x
   y' <- readMaybeDouble y
   return (x', y')


readXyz :: String -> String -> String -> Maybe (Double, Double, Double)
readXyz x y z = do
   x' <- readMaybeDouble x
   y' <- readMaybeDouble y
   z' <- readMaybeDouble z
   return (x', y', z')


asPcbnewEffects :: [SExpr] -> Either String PcbnewAttribute
asPcbnewEffects xs = interpretRest xs []
   where
      interpretRest [] effects = Right (PcbnewFpTextEffects effects)
      interpretRest (sx:sxs) effects = case fromSExpr sx of
         Left err -> Left err
         Right (PcbnewExprAttribute justify@(PcbnewJustify _))
            -> interpretRest sxs (justify:effects)
         Right (PcbnewExprAttribute font@(PcbnewFont _ _ _))
            -> interpretRest sxs (font:effects)
         Right (PcbnewExprAttribute PcbnewHide)
            -> interpretRest sxs (PcbnewHide:effects)
         _ -> expecting "font or justify expression" sx


asPcbnewFont :: [SExpr] -> Either String PcbnewAttribute
asPcbnewFont xs = interpretRest xs defaultPcbnewFont
    where
        interpretRest [] font = Right font
        interpretRest (sx:sxs) font = case fromSExpr sx of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewSize size)) ->
                interpretRest sxs font {pcbnewFontSize = size}
            Right (PcbnewExprAttribute (PcbnewThickness t)) ->
                interpretRest sxs font {pcbnewFontThickness = t}
            Right (PcbnewExprAttribute PcbnewItalic) ->
                interpretRest sxs font {pcbnewFontItalic = True}
            Right _ -> expecting "size, thickness or 'italic'" sx

asXy :: ((Double, Double) -> a) -> [SExpr] -> Either String a
asXy constructor sx@[Atom _ x, Atom _ y] = case readXy x y of
   Just xy -> Right $ constructor xy
   Nothing -> expecting' "two floats (e.g. 1.0 1.0)" sx
asXy _ x = expecting' "two floats (e.g. 1.0 1.0)" x

asPcbnewPts :: [SExpr] -> Either String PcbnewAttribute
asPcbnewPts = fmap PcbnewPts . foldr interpretXys (Right [])
    where interpretXys sx z = case fromSExpr sx of
                        Left err -> Left err
                        Right (PcbnewExprAttribute (PcbnewXy xy))
                            -> Right (xy:) <*> z
                        Right _ -> expecting "'xy' (e.g. '(xy 1.0 1.0)')" sx

asString :: (String -> PcbnewAttribute) -> [SExpr] -> Either String PcbnewAttribute
asString pcbnew [Atom _ s] =  Right $ pcbnew s
asString _ x = expecting' "string" x

asStrings :: ([String] -> PcbnewAttribute) -> [SExpr] -> Either String PcbnewAttribute
asStrings pcbnew atms = Right $ pcbnew (fmap (\(Atom _ s) -> s) atms)

asPcbnewLayers :: [SExpr] -> Either String PcbnewAttribute
asPcbnewLayers [] = Right $ PcbnewLayers []
asPcbnewLayers xs = let layers = map onePcbnewLayer xs in case lefts layers of
    [] -> Right $ PcbnewLayers $ map (\(PcbnewLayer l) -> l) $ rights layers
    _  -> Left $ "Could not fromSExpr layers:\n"
                    ++ unlines (map ("\t\t"++) (lefts layers))

asDouble :: (Double -> PcbnewAttribute) -> [SExpr] -> Either String PcbnewAttribute
asDouble constructor [sx@(Atom _ d)] = case readMaybeDouble d of
   Just d' -> Right $ constructor d'
   Nothing -> expecting "one float (e.g. '1.0')" sx
asDouble _ x = expecting' "one float (e.g. '1.0')" x

asInt :: (Int -> PcbnewAttribute) -> [SExpr] -> Either String PcbnewAttribute
asInt constructor [sx@(Atom _ i)] = case readMaybe i of
   Just i' -> Right $ constructor i'
   Nothing -> expecting "one int (e.g. '1')" sx
asInt _ x = expecting' "one int (e.g. '1')" x

asPcbnewDrill :: [SExpr] -> Either String PcbnewAttribute
asPcbnewDrill xs = interpretRest xs defaultPcbnewDrillT
    where
        interpretRest [] drill = Right $ PcbnewDrill drill
        interpretRest (sx:sxs) drill = case sx of
            Atom _ "oval"  -> interpretRest sxs drill {pcbnewDrillOval = True}
            (List _ _) -> case fromSExpr sx of
                Left err -> Left err
                Right (PcbnewExprAttribute (PcbnewOffset xy))
                    -> interpretRest sxs drill {pcbnewDrillOffset = Just xy}
                Right _ -> expecting "offset or nothing" sx
            Atom _ d  -> case readMaybeDouble d of
                Just d' -> if isNothing (pcbnewDrillSize drill)
                           then interpretRest sxs drill
                                { pcbnewDrillSize = Just (d',d') }
                           else interpretRest sxs drill
                                { pcbnewDrillSize =
                                     fmap (\(x,_) -> (x,d')) (pcbnewDrillSize drill)
                                }
                Nothing -> expecting "float, 'oval' or offset" sx

asPcbnewXyz :: [SExpr] -> Either String PcbnewAttribute
asPcbnewXyz sx@(Atom _ x:Atom _ y:[Atom _ z]) = case readXyz x y z of
    Just xyz -> Right $ PcbnewXyz xyz
    Nothing -> expecting' "three floats" sx
asPcbnewXyz x = expecting' "three floats" x

asXyz :: (PcbnewAttribute -> a) -> [SExpr] -> Either String a
asXyz constructor [l@(List _ _)] = case fromSExpr l of
    Left err -> Left err
    Right (PcbnewExprAttribute xyz) -> Right $ constructor xyz
    Right _ -> expecting "xyz (e.g. '(xyz 1 1 1)')" l
asXyz _ x = expecting' "xyz (e.g. '(xyz 1 1 1)')" x

asPcbnewModel :: [SExpr] -> Either String PcbnewAttribute
asPcbnewModel (Atom _ p:xs) = interpretRest xs defaultPcbnewModel {pcbnewModelPath = p}
    where
        interpretRest [] model = Right model
        interpretRest (sx:sxs) model = case fromSExpr sx of
            Left err -> Left err
            Right (PcbnewExprAttribute (PcbnewModelAt (PcbnewXyz xyz))) ->
                interpretRest sxs model {pcbnewModelAt = xyz}
            Right (PcbnewExprAttribute (PcbnewModelScale (PcbnewXyz xyz))) ->
                interpretRest sxs model {pcbnewModelScale = xyz}
            Right (PcbnewExprAttribute (PcbnewModelRotate (PcbnewXyz xyz))) ->
                interpretRest sxs model {pcbnewModelRotate = xyz}
            Right (PcbnewExprAttribute (PcbnewModelOffset (PcbnewXyz xyz))) ->
                interpretRest sxs model {pcbnewModelRotate = xyz}
            Right (PcbnewExprAttribute PcbnewHide) ->
                interpretRest sxs model {pcbnewModelHide = True}
            Right _ -> expecting "only at, scale, rotate or offset" sx
asPcbnewModel x = expecting' "model path, at, scale, rotate or offset" x

justifyOneOf :: String
justifyOneOf = "one of '"
   ++ concat (intersperse ", " (fmap justifyToString [minBound..]))
   ++ "'"


asPcbnewJustifyT :: [SExpr] -> Either String PcbnewAttribute
asPcbnewJustifyT sx = case lefts js of
   [] -> Right (PcbnewJustify (rights js))
   es -> Left (headOr "" es)
   where js = fmap oneJustifyT sx

oneJustifyT :: SExpr -> Either String PcbnewJustifyT
oneJustifyT sx@(Atom _ s) = case strToJustify s of
   Just j -> Right j
   Nothing -> expecting justifyOneOf sx
oneJustifyT x = expecting justifyOneOf x


asPcbnewOptions :: [SExpr] -> Either String PcbnewAttribute
asPcbnewOptions xs@([clearance, anchor]) =
       case fromSExpr clearance of
         Left err -> Left err
         Right (PcbnewExprAttribute (PcbnewOptionsClearance c)) ->
           case fromSExpr anchor of
                Left err -> Left err
                Right (PcbnewExprAttribute (PcbnewOptionsAnchor a))
                  -> Right (PcbnewOptions c a)
                Right _ -> expecting "anchor shape (e.g. 'rect') but got" anchor
         Right _ -> expecting "clearance type (e.g. 'outline') but got" clearance
asPcbnewOptions xs = expecting' "clearance and anchor" xs

asPcbnewOptionsClearance :: [SExpr] -> Either String PcbnewAttribute
asPcbnewOptionsClearance xs@[Atom _ s] = case strToClearance s of
   Just c -> Right $ PcbnewOptionsClearance c
   Nothing -> expecting' "clearance option (e.g. outline)" xs
asPcbnewOptionsClearance xs = expecting' "clearance option (e.g. outline)" xs

asPcbnewOptionsAnchor :: [SExpr] -> Either String PcbnewAttribute
asPcbnewOptionsAnchor xs@[Atom _ s] = case strToAnchor s of
   Just a -> Right $ PcbnewOptionsAnchor a
   Nothing -> expecting' "anchor shape (e.g. rect)" xs
asPcbnewOptionsAnchor xs = expecting' "anchor shape (e.g. rect)" xs

asPcbnewPrimitives :: [SExpr] -> Either String PcbnewAttribute
asPcbnewPrimitives xs = case asGrItems xs of
            Left err -> Left err
            Right grs -> Right $ PcbnewPrimitives grs

asGrItems :: [SExpr] -> Either String [PcbnewGrItem]
asGrItems xs = if length (lefts grs) /= 0 then Left (head (lefts grs)) else Right $ rights grs
  where grs = fmap oneGrItem xs


oneGrItem :: SExpr -> Either String PcbnewGrItem
oneGrItem (List _ ((Atom _ "gr_poly"):pts:xs)) =
  case fromSExpr pts of
    Left err -> Left err
    Right (PcbnewExprAttribute (PcbnewPts v2s))
      -> interpretRest xs $ defaultPcbnewGrPoly {grPolyPoints = v2s}
        where
          interpretRest [] gr = Right gr
          interpretRest (sx:sxs) gr = case fromSExpr sx of
                Left err -> Left err
                Right (PcbnewExprAttribute (PcbnewWidth w))
                  -> Right $ gr {grItemWidth = w}
                Right (PcbnewExprAttribute (PcbnewGrItemFill))
                  -> Right $ gr {grItemFill = True}
                Right _ -> expecting "width or fill" sx
oneGrItem sx = expecting "points" sx

expecting :: String -> SExpr -> Either String a
expecting x y =
    Left $ "Error in " ++ pos ++ ": expecting " ++ x ++ " but got " ++
        nothing_or (strip_brackets (write y)) ++ " instead"
    where
        nothing_or y' = case y' of
            "" -> "nothing"
            _  -> "'" ++ y' ++ "'"
        strip_brackets y' = case head y' of
                '(' -> tail . init $ y'
                _   -> y'
        pos = show (getPos y)


expecting' :: String -> [SExpr] -> Either String a
expecting' x y = expecting x $ List pos y
  where pos = headOr (newPos "" 0 0) $ fmap getPos y


{- Like readMaybe but allows for '.1' and '-.1' style doubles -}
readMaybeDouble :: String -> Maybe Double
readMaybeDouble str@(c1:c2:rest) = case c1 of
   '.' -> readMaybe ('0':str)
   '-' -> case c2 of
      '.' -> readMaybe ('-':'0':rest)
      _   -> readMaybe str
   _  -> readMaybe str
readMaybeDouble str = readMaybe str
