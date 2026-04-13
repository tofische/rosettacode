import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Data (seqs)
import Lib (forward, inverse, self)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
    describe "Inverse of forward" $ for_ seqs testInverseForward
    describe "Self of self" $ for_ seqs testSelfSelf
    where
        testInverseForward (name, s) = it name $ inverse (forward s) `shouldBe` s
        testSelfSelf (name, s) = it name $ self (self s) `shouldBe` s
