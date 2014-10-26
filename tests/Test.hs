import Test.Framework (Test, defaultMain, testGroup)

import qualified SExpr
import qualified KicadExpr

main :: IO ()
main = defaultMain [ testGroup "SExpr" SExpr.tests
                   , testGroup "KicadExpr" KicadExpr.tests
                   ]

