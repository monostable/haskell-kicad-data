module KicadExpr where
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.AEq
import Data.Maybe

import Data.Kicad.Parse
import Data.Kicad.KicadExpr

tests :: [Test]
tests = [ testProperty "parse fp_line correctly" parseFpLineCorrectly
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
                     ++ fromJust (layerToStr l)
                     ++ ") (width " ++ show d5 ++ "))"
                fpLine  (d1, d2, d3, d4, d5) l = Right $ KicadExprItem
                                               $ KicadFpLine (d1, d2) (d3, d4) l d5
        --parseFpTextCorrectly :: KicadFpTextTypeT
        --                     -> String
        --                     -> KicadAtT
        --                     -> KicadLayerT
        --                     -> (Bool, Bool)
        --                     -> (Double, Double, Double)
        --                     -> Bool
        --parseFpTextCorrectly t s a l bs ds = parse fp_text ~== fpText
        --    where
        --        fp_text = "(fp_text " ++ fromJust (fpTextTypeToStr t)
        --                ++ "\"" ++ s ++ "\"" ++ " (at 0 -3.048) (layer F.SilkS) hid

        --        fpLine  (d1, d2, d3, d4, d5) l = Right $ KicadExprItem
        --                                       $ KicadFpLine (d1, d2) (d3, d4) l d5

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
