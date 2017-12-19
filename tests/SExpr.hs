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

tests :: [Test]
tests = [
        ]

instance Arbitrary SExpr where
    arbitrary = undefined
