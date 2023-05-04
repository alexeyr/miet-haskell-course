import Poly
import SimpleLang
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "poly" $ do
        it "applyPoly" $ pending
    describe "simpleLang" $ do
        -- включите тесты на работу 
        it "desugar" $ pending
