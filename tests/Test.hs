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

main = defaultMain [testGroup "Parse" tests]

tests = [ testCase "parse fp_line correctly" (parse fp_line @=? fpLine)
        , testProperty "parse all keywords" (parseAllKeywords)
        ]
    where
        fp_line = "(fp_line (start 3.302 -0.381) (end 3.302 0.381) (layer F.SilkS) (width 0.127))"
        fpLine  = Right $ KicadExprItem $ KicadFpLine (3.302, -0.381) (3.302, 0.381) FSilkS 0.127
        parseAllKeywords :: Keyword -> Bool
        parseAllKeywords kw = case parseSExpr ("(" ++  show kw ++ ")") of
            res@(Right val) -> res == parseSExpr (show val)
            Left _    -> False

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
                           return $ List $ [AtomKey kw] ++ ls
                      ]

instance Arbitrary Keyword
    where arbitrary = elements $ filter notSpecial [KeyModule .. KeyRectDelta]
            where notSpecial x = not $ elem x specialKeywords

