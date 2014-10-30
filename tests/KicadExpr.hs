{-# OPTIONS_GHC -fno-warn-orphans #-}
module KicadExpr
( tests
)
where
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Control.Monad

import Utils
import Data.Kicad.Parse
import Data.Kicad.KicadExpr
import Data.Kicad.SExpr

tests :: [Test]
tests = [ testProperty "parse fp_line correctly" parseFpLineCorrectly
        , testProperty "parse fp_arc correctly" parseFpArcCorrectly
        , testProperty "parse fp_poly correctly" parseFpPolyCorrectly
        , testProperty "parse and write any attribute" parseAndWriteAnyAttribute
        , testProperty "parse and write any item" parseAndWriteAnyItem
        , testProperty "parse and write any module" parseAndWriteAnyModule
        ]

parseFpLineCorrectly :: (Double, Double, Double, Double, Double)
                     -> KicadLayerT
                     -> Bool
parseFpLineCorrectly (d1, d2, d3, d4, d5) l =
    tracedPropAEq (parse fp_line) fpLine
    where
        fp_line =
            "(fp_line (start " ++ show d1 ++ " "
             ++ show d2 ++ ") (end " ++ show d3 ++ " "
             ++ show d4 ++ ") (layer "
             ++ layerToStr l
             ++ ") (width " ++ show d5 ++ "))"
        fpLine  = Right $ KicadExprItem
                        $ KicadFpLine (d1, d2) (d3, d4) l d5

parseFpArcCorrectly :: (Double, Double, Double, Double, Double) -> Double
                     -> KicadLayerT
                     -> Bool
parseFpArcCorrectly (d1, d2, d3, d4, d5) d6 l =
    tracedPropAEq (parse fp_arc) fpArc
    where
        fp_arc =
            "(fp_arc (start " ++ show d1 ++ " "
            ++ show d2 ++ ") (end " ++ show d3 ++ " "
            ++ show d4 ++ ") (angle " ++ show d5
            ++ ") (layer " ++ layerToStr l ++ ") (width "
            ++ show d6 ++ "))"
        fpArc = Right $ KicadExprItem
                      $ KicadFpArc (d1, d2) (d3, d4) d5 l d6

parseFpPolyCorrectly :: [(Double, Double)] -> Double
                     -> KicadLayerT
                     -> Bool
parseFpPolyCorrectly ds w l =
    tracedPropAEq (parse fp_poly) fpPoly
    where
        fp_poly =
            "(fp_poly (pts "
            ++ unwords
                (map
                  (\(d1,d2) -> "(xy " ++ show d1 ++ " " ++ show d2 ++ ")")
                    ds)
            ++ ") (layer " ++ layerToStr l ++ ") (width "
            ++ show w ++ "))"
        fpPoly = Right $ KicadExprItem $ KicadFpPoly ds l w

parseAndWriteAnyAttribute :: KicadAttribute -> Bool
parseAndWriteAnyAttribute a = tracedPropAEq t1 t2
    where t1 = parse $ write $ toSExpr a
          t2 = Right $ KicadExprAttribute a

parseAndWriteAnyItem :: KicadItem -> Bool
parseAndWriteAnyItem a = tracedPropAEq t1 t2
    where t1 = parse $ write $ toSExpr a
          t2 = Right $ KicadExprItem a

parseAndWriteAnyModule :: KicadModule -> Bool
parseAndWriteAnyModule a = tracedPropAEq t1 t2
    where t1 = parse $ write $ toSExpr a
          t2 = Right $ KicadExprModule a

instance Arbitrary KicadModule where
    arbitrary = do n <- genSafeString
                   l <- arbitrary
                   attrs <- listOf genModuleAttr
                   items <- arbitrary
                   return $ KicadModule n l attrs items
        where
            genModuleAttr :: Gen KicadAttribute
            genModuleAttr = suchThat arbitrary not_layer
            not_layer (KicadLayer _) = False
            not_layer _ = True

instance Arbitrary KicadItem where
    arbitrary = oneof [ do t  <- arbitrary
                           s  <- genSafeString
                           a  <- arbitrary
                           l  <- arbitrary
                           h  <- arbitrary
                           si <- arbitrary
                           th <- arbitrary
                           i  <- arbitrary
                           return $ KicadFpText t s a l h si th i
                      , do s <- arbitrary
                           e <- arbitrary
                           l <- arbitrary
                           w <- arbitrary
                           fp <- elements [KicadFpLine, KicadFpCircle]
                           return $ fp s e l w
                      , do s <- arbitrary
                           e <- arbitrary
                           a <- arbitrary
                           l <- arbitrary
                           w <- arbitrary
                           return $ KicadFpArc s e a l w
                      , do ps <- arbitrary
                           l  <- arbitrary
                           w  <- arbitrary
                           return $ KicadFpPoly ps l w
                      , do n  <- genSafeString
                           t  <- arbitrary
                           s  <- arbitrary
                           a  <- arbitrary
                           si <- arbitrary
                           l  <- arbitrary
                           attrs <- listOf genPadAttrs
                           return $ KicadPad n t s a si l attrs
                      ]

genPadAttrs :: Gen KicadAttribute
genPadAttrs = oneof [ liftM KicadRectDelta        arbitrary
                    , liftM KicadMaskMargin       arbitrary
                    , liftM KicadPasteMarginRatio arbitrary
                    , liftM KicadPasteMargin      arbitrary
                    , liftM KicadClearance        arbitrary
                    , liftM KicadZoneConnect      arbitrary
                    , liftM KicadThermalWidth     arbitrary
                    , liftM KicadThermalGap       arbitrary
                    , do a <- arbitrary
                         b <- arbitrary
                         return $ KicadDrill $ KicadDrillT a True b
                    , do a <- suchThatMaybe arbitrary (uncurry (==))
                         b <- arbitrary
                         return $ KicadDrill $ KicadDrillT a False b
                    ]

instance Arbitrary KicadAttribute where
    arbitrary = oneof [ genPadAttrs
                      , oneof [ liftM KicadLayer     arbitrary
                              , liftM KicadAt        arbitrary
                              , liftM KicadSize      arbitrary
                              , liftM KicadThickness arbitrary
                              , liftM KicadTedit     genSafeString
                              , liftM KicadStart     arbitrary
                              , liftM KicadEnd       arbitrary
                              , liftM KicadCenter    arbitrary
                              , liftM KicadWidth     arbitrary
                              , liftM KicadDescr     genSafeString
                              , liftM KicadTags      genSafeString
                              , liftM KicadAttr      genSafeString
                              , liftM KicadLayers    arbitrary
                              , liftM KicadAngle     arbitrary
                              , liftM KicadXy        arbitrary
                              , liftM KicadPts       arbitrary
                              , liftM KicadXyz       arbitrary
                              , liftM KicadZoneConnect  arbitrary
                              , liftM KicadThermalGap   arbitrary
                              , liftM KicadThermalWidth arbitrary
                              , liftM KicadModelScale   arbitrary
                              , liftM KicadModelRotate  arbitrary
                              , liftM KicadClearance    arbitrary
                              , liftM KicadMaskMargin   arbitrary
                              , liftM KicadPasteMargin  arbitrary
                              , liftM KicadPasteMarginRatio  arbitrary
                              , liftM KicadAutoplaceCost90   arbitrary
                              , liftM KicadAutoplaceCost180  arbitrary
                              , do s <- arbitrary
                                   t <- arbitrary
                                   i <- arbitrary
                                   return $ KicadFont s t i
                              , do p <- genSafeString
                                   a <- arbitrary
                                   s <- arbitrary
                                   r <- arbitrary
                                   return $ KicadModel p a s r
                             ]
                    ]

instance Arbitrary KicadLayerT where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary KicadFpTextTypeT where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary KicadPadShapeT where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary KicadPadTypeT where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary KicadAtT where
    arbitrary = do p <- arbitrary
                   o <- arbitrary
                   return $ KicadAtT p o

