import FunctorsMonads
import Streams hiding (main)
import Test.Hspec
import Data.Either
-- Раскомментируйте QuickCheck или Hegdehog, в зависимости от того, что будете использовать
-- Документация https://hspec.github.io/quickcheck.html
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Classes
import Data.Proxy
-- Документация в https://github.com/parsonsmatt/hspec-hedgehog#readme
-- import Test.Hspec.Hedgehog

-- Добавьте минимум 5 тестов свойств для функций из первых 2 лабораторных (скопируйте определения тестируемых функций сюда).

xor :: Bool -> Bool -> Bool
xor x y = if x == y then False else True

geomProgression :: Double -> Double -> Integer -> Double
geomProgression b q n | (n == 0) = b 
                      | n < 0 = error "n must be a natural number: n >= 2"
                      | n > 0 = q * geomProgression b q (n - 1)  

main :: IO ()
main = hspec $ do
    describe "first steps" $ do
        it "xor" $ do
            xor True True `shouldBe` False
            xor True False `shouldBe` True
            xor False True `shouldBe` True
            xor False False `shouldBe` False
        it "geomProgression" $ do
            geomProgression 5.0 0.5 2 `shouldBe` 1.25
            geomProgression 5.0 2.0 3 `shouldBe` 40.0
    describe "functors and monads" $ do
        it "liftA2'" $ do
            liftA2' (*) (Just 3) (Just 4) `shouldBe` 12
            liftA2' (*) (Just 3) Nothing `shouldBe` Nothing
            liftA2' (+) [1, 2, 3] [4, 5, 6] `shouldBe` [5, 6, 7, 6, 7, 8, 7, 8, 9]  
        it "seqA" $ do 
            seqA [Just 1, Nothing, Just 3] `shouldBe` Nothing
            seqA [Just 1, Just 2, Just 3] `shouldBe` Just [1, 2, 3]
            seqA [[1, 2], [3, 4], [5, 6]] `shouldBe` [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
        it "traverseA" $ do 
            traverseA Just [1, 2, 3] `shouldBe` Just [1, 2, 3]
            traverseA (\x -> if x > 0 then [x, x * 2] else []) [1, 2, 3] `shouldBe` [[1, 2, 3], [1, 2, 4], [1, 2, 6], [1, 3, 3], [1, 3, 6], [1, 6, 3], [1, 6, 6]]
        it "filterA" $ do
            filterA (\a -> if a > 10 then Nothing else Just (a > 0)) [-1, -2, 1, 2] `shouldBe` Just [1, 2]
            filterA (\a -> if a < 0 then Nothing else Just (a > 1)) [-1, -2, 1, 2] `shouldBe` Nothing
            filterA (\x -> if mod x 2 == 0 then Just True else Just False) [1, 2, 3, 4] `shouldBe` Just [2, 4]
            filterA (\x -> if x > 2 then Right (x > 0) else Left False) [2, 4, 6, 8] `shouldBe` Left False
            filterA (\x -> if x > 0 then Right (x > 0) else Left False) [3, 4, 7] `shouldBe` Right [3, 4, 7]
        it "composeM" $ do
            composeM Just Just 5 `shouldBe` Just 5 
            composeM (\x -> if x > 0 then Just (x * 2) else Nothing) (\x -> if x < 10 then Just (x + 1) else Nothing) 5 `shouldBe` Just 12
    describe "Streams" $ do
        it "filterA" $ do
            filterA (\x -> if x > 0 then Just True else Nothing) [1, -2, 3, -4] `shouldBe` Just [1, 3]
        it "streamToList" $ do
            filterA (\x -> if x > 10 then Just True else Nothing) [1, 2, 3, 4] `shouldBe` Nothing
        it "sTake" $ do
            sTake 5 (fromList iterate (+1) 1) `shouldBe` [1, 2, 3, 4, 5]
            sTake 5 (fromList iterate (+2) 2) `shouldBe` [2, 4, 6, 8, 10]
        it "sRepeat" $ do
            sTake 3 (sRepeat 42) `shouldBe` [42, 42, 42]
            sTake 3 (sRepeat True) `shouldBe` [True, True, True]
            sTake 5 (sRepeat (\x -> x + 1)) `shouldBe` [1, 2, 3, 4, 5]
        it "sCycle" $ do
            sTake 7 (sCycle [1, 2, 3]) `shouldBe` [1, 2, 3, 1, 2, 3, 1]
            sTake 5 (sCycle [True, False]) `shouldBe` [True, False, True, False, True]
        it "sIterate" $ do
            sTake 5 (sIterate (\x -> x * 2) 1) `shouldBe` [1, 2, 4, 8, 16]
            sTake 5 (sIterate not True) `shouldBe` [True, False, True, False, True]
        it "nats" $ do
            sTake 5 (nats 1) `shouldBe` [1, 2, 3, 4, 5]
            sTake 5 (nats 0) `shouldBe` [0, 1, 2, 3, 4]
        it "ruler" $ do
            sTake 10 (ruler) `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1]
        it "minMax" $ do
            minMax [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5] `shouldBe` Just (1, 9)
            minMax [10, 20, -5, 30, 5]  `shouldBe`  Just (-5, 30)
            minMax [42]  `shouldBe`  Just (42, 42)
            minMax []  `shouldBe`  Nothing
        it "minMaxBang" $ do  
            minMaxBang [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5] `shouldBe` Just (1, 9)
            minMaxBang [10, 20, -5, 30, 5]  `shouldBe`  Just (-5, 30)
            minMaxBang [42]  `shouldBe`  Just (42, 42)
            minMaxBang []  `shouldBe`  Nothing
