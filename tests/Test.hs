import Test.HUnit hiding (Test)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Control.Monad (liftM)

import Data.Kicad.Parse
import Data.Kicad.KicadExpr
import Data.Kicad.SExpr
import Data.Kicad.ParseSExpr

main :: IO ()
main = defaultMain [testGroup "Parse" tests]

tests :: [Test]
tests = [ testCase "parse fp_line correctly" (parse fp_line @=? fpLine)
        , testProperty "parse all keywords" parseAllKeywords
        ]
    where
        fp_line = "(fp_line (start 3.302 -0.381) (end 3.302 0.381) (layer F.SilkS) (width 0.127))"
        fpLine  = Right $ KicadExprItem $ KicadFpLine (3.302, -0.381) (3.302, 0.381) FSilkS 0.127
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
            where notSpecial x = notElem x specialKeywords

