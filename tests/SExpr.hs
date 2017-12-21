{-# OPTIONS_GHC -fno-warn-orphans #-}
module SExpr
( tests
)
where
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit  (testCase)
import Test.HUnit (assertFailure)
import Test.QuickCheck
import Control.Monad (liftM)
import Data.Either (rights)
import Text.Parsec.Pos (newPos)

import Utils

import Data.Kicad.SExpr

tests :: [Test]
tests = [ testProperty "deterministic 1" deterministic1
        , testCase "allows quote marks in unquoted strings" allowQuoteMarks1
        , testCase "allows quote marks in unquoted strings 2" allowQuoteMarks2
        ]


instance Arbitrary SExpr where
    arbitrary = sized arbitrarySExp


pos = newPos "" 0 0
-- so we don't create infinitely large s-expressions we keep reducing the size
-- as we go deeper and return atoms when the size is 0
arbitrarySExp :: Int -> Gen SExpr
arbitrarySExp n | n > 0 =
    oneof [ arbitraryAtom
          , liftM (List pos) $ resize (n `div` 2) arbitrary
          ]
arbitrarySExp _ = arbitraryAtom


arbitraryAtom :: Gen SExpr
arbitraryAtom = liftM (Atom pos) genSafeString


deterministic1 :: SExpr -> Bool
deterministic1 sx = tracedPropEq t1 t2
        where sx' = List pos [sx]
              t1 = write sx'
              t2 = either id write $ parse t1


allowQuoteMarks1 :: IO ()
allowQuoteMarks1 =
    let sx = parse "(x\")" in
    if sx /= Right (List (newPos "SExpr" 1 1) [Atom (newPos "SExpr" 1 2) "x\""])
    then assertFailure ("could not parse quote mark, got: " ++ show sx)
    else return ()

allowQuoteMarks2 :: IO ()
allowQuoteMarks2 =
    let sx = parse "(xxxx\"yyyy yyyy\"xxxx)"
        expected = Right $ List (newPos "SExpr" 1 1)
            [ Atom (newPos "SExpr" 1 2)"xxxx\"yyyy"
            , Atom (newPos "SExpr" 1 12)"yyyy\"xxxx"
            ]
    in if sx /= expected
       then assertFailure ("could not parse quote mark, got: " ++ show sx)
       else return ()
