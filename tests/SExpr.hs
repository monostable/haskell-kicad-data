{-# OPTIONS_GHC -fno-warn-orphans #-}
module SExpr
( tests
)
where
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Control.Monad (liftM)

import Utils

import Data.Kicad.SExpr
import Data.Kicad.ParseSExpr

tests :: [Test]
tests = [ testProperty "parse all keywords" parseAllKeywords
        ]

parseAllKeywords :: Keyword -> Bool
parseAllKeywords kw = tracedPropEq t1 t2
    where t1 = parseSExpr ("(" ++  write kw ++ ")")
          t2 = parseSExpr $ either id write $ parseSExpr ("(" ++  write kw ++ ")")

instance Arbitrary SExpr where
    arbitrary = oneof [ liftM AtomKey arbitrary
                      , liftM AtomStr genSafeString
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

specialKeywords :: [Keyword]
specialKeywords = [KeyModule, KeyPad, KeyFpText]
