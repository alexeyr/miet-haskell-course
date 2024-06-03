import FunctorsMonads
import Streams hiding (main)
import Test.Hspec
import Data.List
-- Раскомментируйте QuickCheck или Hegdehog, в зависимости от того, что будете использовать
-- Документация https://hspec.github.io/quickcheck.html
-- import Test.Hspec.QuickCheck
-- Документация в https://github.com/parsonsmatt/hspec-hedgehog#readme
-- import Test.Hspec.Hedgehog

-- Добавьте минимум 5 тестов свойств для функций из первых 2 лабораторных (скопируйте определения тестируемых функций сюда).
coprime :: Integer -> Integer -> Bool
coprime a b
  | c == 0 || d == 0 = False
  | c == 1 || d == 1 = True
  | c > d = coprime (c - d) d
  | otherwise = coprime (d - c) c
  where c = abs a
        d = abs b
        
max3, median3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = if max x y >= z then max x y else z
median3 x y z = if max x y >= z then (if x >= y then max y z else max x z) else max x y

newtype Poly a = Poly [a]
xPows x = map (\n -> x**n) [0..]
getCoefs (Poly p) = p
discardZeroes [] = []
discardZeroes p = if (last p /= 0) then p 
                  else discardZeroes $ init p
makePoly p x = sum $ zipWith (*) (getCoefs p) (xPows x)
applyPoly p x = makePoly p x

main :: IO ()
main = hspec $ do
    describe "FunctorsMonads" $ do
    	it "<$$>" $ do
    	    (+1) <$$> Nothing `shouldBe` Nothing
  	    (*2) <$$> Just 1 `shouldBe` Just 2
  	it "join'" $ do
  	    join' Nothing = Nothing
  	    join' (Just [1, 2, 3]) = [1, 2, 3]
        it "liftA2'" $ do
            liftA2' (+) Nothing Nothing `shouldBe` Nothing
            liftA2' (+) Nothing (Just 1) `shouldBe` Nothing
            liftA2' (+) (Just 2) (Just 1) `shouldBe` Just 3
        it "seqA" $ do 
            seqA [Nothing, Nothing] `shouldBe` Nothing
            seqA [Just 1, Nothing] `shouldBe` Nothing
            seqA [Just 1, Just 2] `shouldBe` Just [1, 2]
            seqA [[1, 2], [3, 4]] `shouldBe` [[1,3],[1,4],[2,3],[2,4]]
        it "traverseA" $ do 
            traverseA Just [1, 2] `shouldBe` Just [1, 2]
            traverseA (*2) [1, 2] `shouldBe` [2, 4]
        it "filterA" $ do
            filterA (\x -> if x > 10 then Nothing else Just (x > 0)) [-2, -1, 0, 1, 2] `shouldBe` Just [1, 2]
            filterA (\x -> if x > 0 then Nothing else Just (x > 0)) [-2, -1, 1, 2] `shouldBe` Nothing
        it "composeM" $ do
            composeM Just Just 1 `shouldBe` Just 1 
            composeM (*2) (+1) 3 `shouldBe` Just 8
    describe "Streams" $ do
        it "sTake" $ do
            sTake 0 (sIterate (+1) 1) `shouldBe` []
            sTake 3 (sIterate (+1) 1) `shouldBe` [1, 2, 3]
        it "sRepeat" $ do
            sTake 5 (sRepeat 1) `shouldBe` [1, 1, 1, 1, 1]
        it "sCycle" $ do
            sTake 4 (sCycle [1, 2]) `shouldBe` [1, 2, 1, 2]
        it "sIterate" $ do
            sTake 5 (sIterate (*2) 1) `shouldBe` [2, 4, 6, 8, 10]
            sTake 5 (sIterate not True) `shouldBe` [True, False, True, False, True]
        it "nats" $ do
            sTake 5 nats `shouldBe` [0, 1, 2, 3, 4]
        it "ruler" $ do
            sTake 5 (ruler) `shouldBe` [0, 1, 0, 2, 0]
        it "minMax" $ do
            minMax []  `shouldBe`  Nothing
            minMax [1]  `shouldBe`  Just (42, 42)
            minMax [-10, 15, -25, 70, 1] `shouldBe` Just (-25, 70)
            
