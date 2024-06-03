import Poly
import SimpleLang
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "poly" $ do
        it "applyPoly" $ do
            applyPoly (P [0, 0, 0]) 10 `shouldBe` 0
            applyPoly (P [4, -4, 1 ]) 2 `shouldBe` 0
        it "+" $ do 
            (P [2, 0, 6]) + (P []) `shouldBe` (P [2, 0, 6])
            (P [4, 5]) + (P [1, 1, 1]) `shouldBe` (P [5, 6, 1])
        it "*" $ do
            (P [1, 2, 3, 4, 5]) * (P [5, 4, 3, 2, 1]) `shouldBe` (P [5, 14, 26, 40, 55, 40, 26, 14, 5])
        it "negate" $ do 
            negate (P [0, 0, 0]) `shouldBe` P [0, 0, 0]
            negate (P [5, -5, 3]) `shouldBe` P [-5, 5, -3]
        it "(==)" $ do 
            ((P [4, 1, 7]) == (P [4, 1, 5])) `shouldBe` False
            ((P [5, 3]) == (P [5, 3, 0, 0, 0])) `shouldBe` True
        it "show" $ do 
            show (P [0 , 6, 3]) `shouldBe` "3 * x^2 + 6 * x"
        it "nderiv" $ do 
            nderiv 2 (P [10, 5, 2, 1]) `shouldBe` (P [4, 6])
    describe "simpleLang" $ do
        -- включите тесты на работу 
        it "extend" $ do  
            extend empty "a" 1 "a" `shouldBe` 1
        it "eval" $ do  
            eval (extend empty "a" 4) (Op (Var "a") Plus (Val 1)) `shouldBe` 5
            eval (extend empty "a" 9) (Op (Var "a") Minus (Val 2)) `shouldBe` 7
            eval (extend empty "a" 6) (Op (Var "a") Divide (Val 2)) `shouldBe` 3
            eval (extend empty "a" 5) (Op (Var "a") Gt (Val 1)) `shouldBe` 1
            eval (extend empty "a" 5) (Op (Var "a") Ge (Val 5)) `shouldBe` 1
            eval (extend empty "a" 2) (Op (Var "a") Lt (Val 1)) `shouldBe` 0
            eval (extend empty "a" 2) (Op (Var "a") Le (Val 2)) `shouldBe` 1
            eval (extend empty "a" 1) (Op (Var "a") Eql (Val 1)) `shouldBe` 1
        it "desugar" $ do  
            desugar (Incr "a") `shouldBe` DAssign "a" (Op (Var "a") Plus (Val 1))
        it "programms" $ do  
            ((SimpleLang.run (extend empty "In" 9) fibonacci) "Out") `shouldBe` 55
            ((SimpleLang.run (extend empty "A" 81) squareRoot) "B") `shouldBe` 9
