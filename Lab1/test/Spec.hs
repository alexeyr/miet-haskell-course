{-# OPTIONS_GHC -Wno-type-defaults #-}
import FirstSteps
import Lists
import Luhn
import Test.Hspec
import Control.Exception.Base (tryJust)

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
        it "median3" $ do
            median3 1 3 2 `shouldBe` 2
            median3 5 2 5 `shouldBe` 5
        it "rbgToCmyk" $ do
            let cmyk = rbgToCmyk (RGB 10 20 30)
            cmyk `shouldBe` CMYK { cyan = 0.666666666666667, magenta = 0.333333333333334, yellow = 0.0, black = 0.8823529411764706 }
        it "geomProgression" $ do
            geomProgression 3.0 2.0 2 `shouldBe` 12.0
        it "coprime" $  do
            coprime 10 24 `shouldBe` False
            coprime 17 3 `shouldBe` True
            coprime 1 142 `shouldBe` True
    describe "lists" $ do
        it "distance" $ do
            distance (Point [1,0,1,0,1,1]) (Point [0,1,0,0,1,0]) `shouldBe` 2
            distance (Point [3,4,0]) (Point [0,0,sqrt 11]) `shouldBe` 6
            distance (Point [3,0]) (Point [2,sqrt 8]) `shouldBe` 3.0000000000000004
        it "intersect" $ do
            intersect [1,2,3,4] [1,2,5,11,21] `shouldBe` [1,2]
            intersect [1,1,1] [1,2,1,5,6] `shouldBe` [1,1]
        it "zipN" $ do
            zipN [[1,2,3],[4,5],[10]] `shouldBe` [[1,4,10],[2,5],[3]]
            zipN [[1,2,3],[9],[]] `shouldBe` [[1,9],[2],[3]]
        it "find" $ do
            find (<0) [1,2,10,-1,41] `shouldBe` Just (-1)
        it "findFilter" $ do
            findFilter (<0) [1,2,10,-1,41] `shouldBe` Just (-1)
        it "findLast" $ do
            findLast (<0) [1,2,-1,-2,-3] `shouldBe` Just (-3)
            findLast (>0) [-1,-10] `shouldBe` Nothing
        it "mapFuncs" $ do
            mapFuncs [\x -> x*x, \x -> x - 2, \x -> if even x then 1 else 0] 11 `shouldBe` [121, 9, 0]
        it "satisfiesAll" $ do
            satisfiesAll [even, \x -> x `rem` 5 == 0, (>10)] 12 `shouldBe` False
            satisfiesAll [even, \x -> x `rem` 5 == 0, (>10)] 20 `shouldBe` True
            satisfiesAll [] 12 `shouldBe` True
        it "tailNel" $ do
            tailNel (NEL 1 [1,2,3,4]) `shouldBe` [1,2,3,4]
        it "lastNel" $ do 
            lastNel (NEL 10 [4,2,1,2]) `shouldBe` 2
        it "zipNel" $ do
            zipNel (NEL 1 [1,2,3]) (NEL 11 [41,2,11]) `shouldBe` NEL (1,11) [(1,41),(2,2),(3,11)]
        it "listToNel" $ do
            listToNel [1,2,3,4,5] `shouldBe` Just (NEL 1 [2,3,4,5])
        it "nelToList" $ do
            nelToList (NEL 1 [2,3,4,5]) `shouldBe` [1,2,3,4,5]
    describe "luhn" $ it "luhn" $ do
        isLuhnValid 4276610017565479 `shouldBe` True
        isLuhnValid 4377727816025071 `shouldBe` True
