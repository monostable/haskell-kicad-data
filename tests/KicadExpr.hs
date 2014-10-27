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

instance Arbitrary KicadAttribute where
    arbitrary = oneof [ liftM KicadLayer     arbitrary
                      , liftM KicadAt        arbitrary
                      , liftM KicadSize      arbitrary
                      , liftM KicadThickness arbitrary
                      , liftM KicadTedit     genSafeString
                      , liftM KicadStart     arbitrary
                      , liftM KicadEnd       arbitrary
                      , liftM KicadWidth     arbitrary
                      , liftM KicadDescr     genSafeString
                      , liftM KicadTags      genSafeString
                      , liftM KicadAttr      genSafeString
                      , liftM KicadLayers    arbitrary
                      , liftM KicadDrill     arbitrary
                      , liftM KicadAngle     arbitrary
                      , liftM KicadXy        arbitrary
                      , liftM KicadPts       arbitrary
                      , do s <- arbitrary
                           t <- arbitrary
                           i <- arbitrary
                           return $ KicadFont s t i
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

