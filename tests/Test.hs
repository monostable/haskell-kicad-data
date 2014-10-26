import Test.HUnit hiding (Test)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Control.Monad (liftM)
import Data.AEq
import Data.Maybe

import Data.Kicad.Parse
import Data.Kicad.KicadExpr
import Data.Kicad.SExpr
import Data.Kicad.ParseSExpr

import Debug.Trace

main :: IO ()
main = defaultMain [testGroup "Parse" tests]

tests :: [Test]
tests = [ testProperty "parse fp_line correctly" parseFpLineCorrectly
        , testProperty "parse all keywords" parseAllKeywords
        ]
    where
        parseFpLineCorrectly :: (Double, Double, Double, Double, Double)
                             -> KicadLayerT
                             -> Bool
        parseFpLineCorrectly ds l = parse (fp_line ds l) ~== fpLine ds l
            where
                fp_line (d1, d2, d3, d4, d5) l =
                    "(fp_line (start "
                     ++ show d1 ++ " "
                     ++ show d2 ++ ") (end "
                     ++ show d3 ++ " "
                     ++ show d4 ++ ") (layer "
                     ++ fromJust (layerToStr l)
                     ++ ") (width " ++ show d5 ++ "))"
                fpLine :: (Double, Double, Double, Double, Double)
                       -> KicadLayerT
                       -> Either String KicadExpr
                fpLine  (d1, d2, d3, d4, d5) l = Right $ KicadExprItem
                                               $ KicadFpLine (d1, d2) (d3, d4) l d5
        parseAllKeywords :: Keyword -> Bool
        parseAllKeywords kw = case parseSExpr ("(" ++  write kw ++ ")") of
            res@(Right sx) -> res == parseSExpr (write sx)
            Left _    -> False

specialKeywords :: [Keyword]
specialKeywords = [KeyModule, KeyPad, KeyFpText]

instance Arbitrary SExpr where
    arbitrary = oneof [ liftM AtomKey arbitrary
                      , liftM AtomStr arbitrary
                      , liftM AtomDbl arbitrary
                      , do ls <- arbitrary
                           kw <- elements specialKeywords
                           return $ List $ [AtomKey kw, AtomStr "a", AtomStr "b", AtomStr "c"] ++ ls
                      , do ls <- arbitrary
                           kw <- arbitrary
                           return $ List $ AtomKey kw : ls
                      ]

instance Arbitrary Keyword
    where arbitrary = elements $ filter notSpecial [KeyModule .. KeyRectDelta]
            where notSpecial x = x `notElem` specialKeywords


instance Arbitrary KicadLayerT
    where arbitrary = elements [FSilkS .. AllMask]
