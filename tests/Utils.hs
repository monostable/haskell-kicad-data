module Utils where
import Data.AEq
import Debug.Trace
import Test.QuickCheck

genSafeString :: Gen String
genSafeString = listOf genSafeChar
    where genSafeChar :: Gen Char
          genSafeChar = elements [' '..'\126']

tracedPropAEq :: (Show a, AEq a) => a -> a -> Bool
tracedPropAEq = tracedProp (~==) "APPROX. EQUAL"

tracedPropEq :: (Show a, Eq a) => a -> a -> Bool
tracedPropEq = tracedProp (==) "EQUAL"

tracedProp :: Show a => (a -> a -> Bool) -> String -> a -> a -> Bool
tracedProp fn s t1 t2 = fn t1 t2 ||
                            trace (    "====================================="
                                     ++ "=====================================\n"
                                     ++ show t1 ++ "\n"
                                     ++ " - DOES NOT " ++ s ++ " -\n"
                                     ++ show t2 ++ "\n"
                                     ++ "====================================="
                                     ++ "====================================="
                                   ) False
