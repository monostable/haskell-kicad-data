{-# OPTIONS_GHC -fno-warn-orphans #-}
module SExpr
( tests
)
where
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Control.Monad (liftM)
import Data.Either (rights)

import Utils

import Data.Kicad.SExpr

tests :: [Test]
tests = [ testProperty "deterministic 1" deterministic1
        , testProperty "deterministic 2" deterministic2
        ]


instance Arbitrary SExpr where
    arbitrary = sized arbitrarySExp


-- so we don't create infinitely large s-expressions we keep reducing the size
-- as we go deeper and return atoms when the size is 0
arbitrarySExp :: Int -> Gen SExpr
arbitrarySExp n | n > 0 =
    oneof [ arbitraryAtom
          , liftM List $ resize (n `div` 2) arbitrary
          ]
arbitrarySExp _ = arbitraryAtom


arbitraryAtom :: Gen SExpr
arbitraryAtom = liftM Atom genSafeString


deterministic1 :: SExpr -> Bool
deterministic1 sx = tracedPropEq t1 t2
        where sx' = List [sx]
              t1 = write sx'
              t2 = either id write $ parse t1


deterministic2 :: SExpr -> Bool
deterministic2 sx = tracedPropEq t1 t2
        where sx' = List [sx]
              t1 = Right sx'
              t2 = parse $ write $ head  $ rights [t1]
