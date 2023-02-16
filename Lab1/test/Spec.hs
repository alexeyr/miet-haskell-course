import FirstSteps
import Lists
import Luhn
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "first steps" $ do
        -- Можно вложить глубже: describe "xor" do $ ... чтобы дать названия отдельным тестам
        it "xor" $ do
            xor True True `shouldBe` False
            xor True False `shouldBe` True
            xor False True `shouldBe` True
            xor False False `shouldBe` False
        it "max3" $ do
            max3 1 3 2 `shouldBe` 3
            max3 5 2 5 `shouldBe` 5
        it "median3" pending
        it "rbgToCmyk" pending
        it "geomProgression" pending
        it "coprime" pending
    describe "lists" $ do
        it "distance" pending
        it "intersect" pending
        it "zipN" pending
        it "find" pending
        it "findLast" pending
        it "mapFuncs" pending
        it "tailNel" pending
        it "lastNel" pending
        it "zipNel" pending
        it "listToNel" pending
        it "nelToList" pending
    describe "luhn" $ it "" pending
