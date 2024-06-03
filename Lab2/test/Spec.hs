import Poly
import SimpleLang
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "poly" $ do
        it "applyPoly" $ do
            applyPoly (Poly []) 1 `shouldBe` 0
            applyPoly (Poly [2]) 1 `shouldBe` 2
            applyPoly (Poly [1, 2, 3]) 2 `shouldBe` 17.0
        it "+" $ do 
            (Poly [1, 2]) + (Poly []) `shouldBe` (Poly [1, 2])
            (P [1, 2, 3]) + (P [1, 2]) `shouldBe` (P [2, 4, 3])
        it "*" $ do
            (P [1, 2, 3]) * (P []) `shouldBe` (P [0, 0, 0])
            (P [1, 2, 3]) * (P [1, 2]) `shouldBe` (P [1, 4, 7, 6])
        it "negate" $ do 
            negate (P []) `shouldBe` P []
            negate (P [1, -2, 3]) `shouldBe` P [-1, 2, -3]
        it "(==)" $ do 
            ((P [0, 1, 0]) == (P [0, 1])) `shouldBe` True
            ((P [1, 2, 3]) == (P [1, 2])) `shouldBe` False
			((P [1, 2, 3]) == (P [1, 2, 4])) `shouldBe` False
        it "show" $ do 
            show (P [1, 2, 3]) `shouldBe` "3 * x^2 + 2 * x + 1"
        it "nderiv" $ do 
            nderiv 0 (P [1, 2, 3]) `shouldBe` (P [1, 2, 3])
            nderiv 1 (P [1, 2, 3]) `shouldBe` (P [2, 6])
            nderiv 2 (P [1, 2, 3]) `shouldBe` (P [6])
            nderiv 3 (P [1, 2, 3]) `shouldBe` (P []) 
    describe "simpleLang" $ do
        -- включите тесты на работу 
        it "extend" $ do  
            extend empty "a" 1 "a" `shouldBe` 1
        it "eval" $ do  
            eval (extend empty "a" 1) (Op (Var "a") Plus (Val 1)) `shouldBe` 2
            eval (extend empty "a" 2) (Op (Var "a") Minus (Val 1)) `shouldBe` 1
            eval (extend empty "a" 6) (Op (Var "a") Divide (Val 3)) `shouldBe` 2
            eval (extend empty "a" 2) (Op (Var "a") Gt (Val 1)) `shouldBe` 1
            eval (extend empty "a" 2) (Op (Var "a") Ge (Val 2)) `shouldBe` 1
            eval (extend empty "a" 2) (Op (Var "a") Lt (Val 1)) `shouldBe` 0
            eval (extend empty "a" 2) (Op (Var "a") Le (Val 2)) `shouldBe` 1
            eval (extend empty "a" 1) (Op (Var "a") Eql (Val 1)) `shouldBe` 1
        it "desugar" $ do  
            desugar (Incr "a") `shouldBe` DAssign "a" (Op (Var "a") Plus (Val 1))
        it "programms" $ do  
            ((SimpleLang.run (extend empty "In" 7) fibonacci) "Out") `shouldBe` 21
            ((SimpleLang.run (extend empty "A" 49) squareRoot) "B") `shouldBe` 7
