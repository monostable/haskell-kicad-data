{-# OPTIONS_GHC -fno-warn-orphans #-}
module PcbnewExpr
( tests
)
where
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Control.Monad

import Utils
import Data.Kicad.PcbnewExpr
import Data.Kicad.PcbnewExpr.PcbnewExpr

tests :: [Test]
tests = [ testProperty "parse fp_line correctly" parseFpLineCorrectly
        , testProperty "parse fp_arc correctly" parseFpArcCorrectly
        , testProperty "parse fp_poly correctly" parseFpPolyCorrectly
        , testProperty "parse and write any PcbnewExpr" parseAndWriteAnyPcbnewExpr
        , testProperty "parse and pretty any PcbnewExpr" parseAndPrettyAnyPcbnewExpr
        ]

parseFpLineCorrectly :: (Double, Double, Double, Double, Double)
                     -> PcbnewLayerT
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
        fpLine  = Right $ PcbnewExprItem
                        $ PcbnewFpLine (d1, d2) (d3, d4) l d5

parseFpArcCorrectly :: (Double, Double, Double, Double, Double) -> Double
                     -> PcbnewLayerT
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
        fpArc = Right $ PcbnewExprItem
                      $ PcbnewFpArc (d1, d2) (d3, d4) d5 l d6

parseFpPolyCorrectly :: [(Double, Double)] -> Double
                     -> PcbnewLayerT
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
        fpPoly = Right $ PcbnewExprItem $ PcbnewFpPoly ds l w

parseAndWriteAnyPcbnewExpr :: PcbnewExpr -> Bool
parseAndWriteAnyPcbnewExpr a = tracedPropAEq t1 t2
    where t1 = parse $ write a
          t2 = Right a

parseAndPrettyAnyPcbnewExpr :: PcbnewExpr -> Bool
parseAndPrettyAnyPcbnewExpr a = tracedPropAEq t1 t2
    where t1 = parse $ show $ pretty a
          t2 = Right a

instance Arbitrary PcbnewExpr where
    arbitrary = oneof [ do a <- arbitrary
                           return $ PcbnewExprModule a
                      , do a <- arbitrary
                           return $ PcbnewExprItem a
                      , do a <- arbitrary
                           return $ PcbnewExprAttribute a
                      ]

instance Arbitrary PcbnewModule where
    arbitrary = do n <- genSafeString
                   l <- arbitrary
                   attrs <- listOf genModuleAttr
                   items <- arbitrary
                   return $ PcbnewModule n l attrs items
        where
            genModuleAttr :: Gen PcbnewAttribute
            genModuleAttr = suchThat arbitrary not_layer
            not_layer (PcbnewLayer _) = False
            not_layer _ = True

instance Arbitrary PcbnewItem where
    arbitrary = oneof [ do t  <- arbitrary
                           s  <- genSafeString
                           a  <- arbitrary
                           l  <- arbitrary
                           h  <- arbitrary
                           si <- arbitrary
                           th <- arbitrary
                           i  <- arbitrary
                           return $ PcbnewFpText t s a l h si th i
                      , do s <- arbitrary
                           e <- arbitrary
                           l <- arbitrary
                           w <- arbitrary
                           fp <- elements [PcbnewFpLine, PcbnewFpCircle]
                           return $ fp s e l w
                      , do s <- arbitrary
                           e <- arbitrary
                           a <- arbitrary
                           l <- arbitrary
                           w <- arbitrary
                           return $ PcbnewFpArc s e a l w
                      , do ps <- arbitrary
                           l  <- arbitrary
                           w  <- arbitrary
                           return $ PcbnewFpPoly ps l w
                      , do n  <- genSafeString
                           t  <- arbitrary
                           s  <- arbitrary
                           a  <- arbitrary
                           si <- arbitrary
                           l  <- arbitrary
                           attrs <- listOf genPadAttrs
                           return $ PcbnewPad n t s a si l attrs
                      ]

genPadAttrs :: Gen PcbnewAttribute
genPadAttrs = oneof [ liftM PcbnewRectDelta        arbitrary
                    , liftM PcbnewMaskMargin       arbitrary
                    , liftM PcbnewPasteMarginRatio arbitrary
                    , liftM PcbnewPasteMargin      arbitrary
                    , liftM PcbnewClearance        arbitrary
                    , liftM PcbnewZoneConnect      arbitrary
                    , liftM PcbnewThermalWidth     arbitrary
                    , liftM PcbnewThermalGap       arbitrary
                    , do a <- arbitrary
                         b <- arbitrary
                         return $ PcbnewDrill $ PcbnewDrillT a True b
                    , do a <- suchThatMaybe arbitrary (uncurry (==))
                         b <- arbitrary
                         return $ PcbnewDrill $ PcbnewDrillT a False b
                    ]

instance Arbitrary PcbnewAttribute where
    arbitrary = oneof [ genPadAttrs
                      , oneof [ liftM PcbnewLayer     arbitrary
                              , liftM PcbnewAt        arbitrary
                              , liftM PcbnewSize      arbitrary
                              , liftM PcbnewThickness arbitrary
                              , liftM PcbnewTedit     genSafeString
                              , liftM PcbnewStart     arbitrary
                              , liftM PcbnewEnd       arbitrary
                              , liftM PcbnewCenter    arbitrary
                              , liftM PcbnewWidth     arbitrary
                              , liftM PcbnewDescr     genSafeString
                              , liftM PcbnewTags      genSafeString
                              , liftM PcbnewAttr      genSafeString
                              , liftM PcbnewLayers    arbitrary
                              , liftM PcbnewAngle     arbitrary
                              , liftM PcbnewXy        arbitrary
                              , liftM PcbnewPts       arbitrary
                              , liftM PcbnewXyz       arbitrary
                              , liftM PcbnewZoneConnect  arbitrary
                              , liftM PcbnewThermalGap   arbitrary
                              , liftM PcbnewThermalWidth arbitrary
                              , liftM PcbnewModelScale   arbitrary
                              , liftM PcbnewModelRotate  arbitrary
                              , liftM PcbnewClearance    arbitrary
                              , liftM PcbnewMaskMargin   arbitrary
                              , liftM PcbnewPasteMargin  arbitrary
                              , liftM PcbnewPasteMarginRatio  arbitrary
                              , liftM PcbnewAutoplaceCost90   arbitrary
                              , liftM PcbnewAutoplaceCost180  arbitrary
                              , do s <- arbitrary
                                   t <- arbitrary
                                   i <- arbitrary
                                   return $ PcbnewFont s t i
                              , do p <- genSafeString
                                   a <- arbitrary
                                   s <- arbitrary
                                   r <- arbitrary
                                   return $ PcbnewModel p a s r
                             ]
                    ]

instance Arbitrary PcbnewLayerT where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary PcbnewFpTextTypeT where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary PcbnewPadShapeT where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary PcbnewPadTypeT where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary PcbnewAtT where
    arbitrary = do p <- arbitrary
                   o <- arbitrary
                   return $ PcbnewAtT p o

