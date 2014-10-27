module SExpr where
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Control.Monad (liftM)

import Data.Kicad.SExpr
import Data.Kicad.ParseSExpr

tests :: [Test]
tests = [ testProperty "parse all keywords" parseAllKeywords
        ]
    where
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
    where arbitrary = elements $ filter notSpecial [minBound .. maxBound]
            where notSpecial x = notElem x specialKeywords
