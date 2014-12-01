import Test.Framework (defaultMain, testGroup)

import qualified SExpr
import qualified PcbnewExpr

main :: IO ()
main = defaultMain [ testGroup "SExpr" SExpr.tests
                   , testGroup "PcbnewExpr" PcbnewExpr.tests
                   ]
