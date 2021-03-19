import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Exp
import Data.Maybe (isJust)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Properties"
             [
               QC.testProperty "Generates only typed expressions" $
                  \ e -> isJust (tc e)
             ]
