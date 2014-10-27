module KicadExpr where
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.AEq
import Data.Maybe
import Control.Monad

import Data.Kicad.Parse
import Data.Kicad.KicadExpr
import Data.Kicad.SExpr

import Debug.Trace

tests :: [Test]
tests = [ testProperty "parse fp_line correctly" parseFpLineCorrectly
        , testProperty "parse attributes"        parseAttribute
        --, testProperty "parse layer"             parseLayer
        --, testProperty "parse size"              parseSize
        ]
    where
        parseFpLineCorrectly :: (Double, Double, Double, Double, Double)
                             -> KicadLayerT
                             -> Bool
        parseFpLineCorrectly ds l = parse (fp_line ds l) ~== fpLine ds l
            where
                fp_line (d1, d2, d3, d4, d5) l =
                    "(fp_line (start " ++ show d1 ++ " "
                     ++ show d2 ++ ") (end " ++ show d3 ++ " "
                     ++ show d4 ++ ") (layer "
                     ++ layerToStr l
                     ++ ") (width " ++ show d5 ++ "))"
                fpLine  (d1, d2, d3, d4, d5) l = Right $ KicadExprItem
                                               $ KicadFpLine (d1, d2) (d3, d4) l d5
        parseAttribute :: KicadAttribute -> Bool
        parseAttribute a = if not (t1 ~== t2) then trace (either id show t1 ++ "\n\tDOES NOT EQUAL\n" ++  either id show t2) False else True
            where t1 = parse (write (toSExpr a))
                  t2 = Right (KicadExprAttribute a)
        --parseLayer :: KicadLayerT -> Bool
        --parseLayer x =
        --    parse (write (toSExpr x)) ~== Right (KicadExprAttribute $ KicadLayer x)
        --parseAt :: KicadAtT -> Bool
        --parseAt x =
        --    parse (write (toSExpr x)) ~== Right (KicadExprAttribute $ KicadAt x)
        --parseSize :: (Double, Double) -> Bool
        --parseSize x =
        --    parse (write (toSExpr (KicadSize x))) ~== Right (KicadExprAttribute $ KicadSize x)
        --parseFpTextCorrectly :: KicadFpTextTypeT
        --                     -> String
        --                     -> KicadAtT
        --                     -> KicadLayerT
        --                     -> (Bool, Bool)
        --                     -> (Double, Double, Double)
        --                     -> Bool
        --parseFpTextCorrectly t s a l bs ds = parse fp_text ~== fpText
        --    where
        --        fp_text = "(fp_text " ++ fpTextTypeToStr t
        --                ++ "\"" ++ s ++ "\"" ++ " (at 0 -3.048) (layer F.SilkS) hid

        --        fpLine  (d1, d2, d3, d4, d5) l = Right $ KicadExprItem
        --                                       $ KicadFpLine (d1, d2) (d3, d4) l d5

instance Arbitrary KicadLayerT where
    arbitrary = arbitraryBoundedEnum

genSafeChar :: Gen Char
genSafeChar = elements ['0'..'z']

genSafeString :: Gen String
genSafeString = listOf genSafeChar

instance Arbitrary KicadAttribute where
    arbitrary = oneof [ liftM KicadLayer     arbitrary
                      , liftM KicadAt        arbitrary
                      , liftM KicadSize      arbitrary
                      , liftM KicadThickness arbitrary
                      , liftM KicadTEdit     genSafeString
                      , liftM KicadStart     arbitrary
                      , liftM KicadEnd       arbitrary
                      , liftM KicadWidth     arbitrary
                      , liftM KicadDescr     genSafeString
                      , liftM KicadTags      genSafeString
                      , liftM KicadAttr      genSafeString
                      , liftM KicadLayers    arbitrary
                      , liftM KicadDrill     arbitrary
                      , do s <- arbitrary
                           t <- arbitrary
                           i <- arbitrary
                           return $ KicadFont s t i
                      ]

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
