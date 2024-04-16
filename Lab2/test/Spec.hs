import Poly
import SimpleLang
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "applyPoly" $ do
    it "applyPoly_1" $
      applyPoly x 2 `shouldBe` 2
    it "applyPoly_2" $
      applyPoly (P [1, 2, 3]) 3 `shouldBe` 34
  describe "Poly equality" $ do
    it "equality_1" $
      P [1, 2, 3] `shouldBe` P [1, 2, 3, 0]
    it "equality_2" $
      P [0, 1, 2] `shouldBe` P [0, 1, 2, 0, 0]
    it "equality_3" $
      P [0, 1, 2] `shouldNotBe` P [0, 1, 3]
  describe "showPoly" $ do
    it "show_1" $
      show (P [1, 2, 3, 0, 0]) `shouldBe` "3x^2 + 2x + 1"
    it "show_2" $
      show (P [-1, 2, 3]) `shouldBe` "3x^2 + 2x - 1"
    it "show_3" $
      show (P [0]) `shouldBe` ""
  describe "plus" $ do
    it "plus_1" $
      plus (P [1, 2, 3, 0]) (P [4, 5, 1, 21]) `shouldBe` P [5, 7, 4, 21]
    it "plus_2" $ do
      plus (P [1, 2, 3, 0]) (P []) `shouldBe` P [1, 2, 3, 0]
    it "plus_3" $ do
      plus (P [1, 2, 3, 0]) (P [4, 5]) `shouldBe` P [5, 7, 3, 0]
  describe "times" $ do
    it "times_1" $
      times (P [1, 2, 3]) (P [4, 5, 6]) `shouldBe` P [4, 13, 28, 27, 18]
    it "times_2" $
      times (P []) (P [4, 5, 6]) `shouldBe` P []
    it "times_3" $
      times (P [1, 2, 3]) (P []) `shouldBe` P []
    it "times_4" $
      times (P [1]) (P [4, 5, 6]) `shouldBe` P [4, 5, 6]
    it "times_5" $
      times (P [2, 5, 1]) (P [3, 1]) `shouldBe` P [6, 17, 8, 1]
    it "times_6" $
      times (P [4, 2, 1]) (P [1, 7, 1]) `shouldBe` P [4, 30, 19, 9, 1]
  describe "deriv" $ do
    it "deriv_1" $
      deriv (P [1]) `shouldBe` P [0]
    it "deriv_2" $
      deriv (P [1, 10]) `shouldBe` P [10]
    it "deriv_3" $
      deriv (P [1, 2, 3, 4]) `shouldBe` P [2, 6, 12]
  describe "nderiv" $ do
    it "nderiv_1" $
      nderiv 4 (P [1, 2, 3, 4, 5]) `shouldBe` P [120]
    it "nderiv_2" $
      nderiv 5 (P [1, 2, 3, 4, 5]) `shouldBe` P [0]
    it "nderiv_3" $
      nderiv 3 (P [1, 2, 5, 1, 1, 1]) `shouldBe` P [6, 24, 60]
  describe "simpleLang" $ do
    it "eval_1" $ do
      eval (extend empty "a" 2) (Op (Val 3) Plus (Var "a")) `shouldBe` 5
    it "eval_2" $ do
      eval (extend empty "a" 5) (Op (Val 3) Minus (Var "a")) `shouldBe` -2
    it "eval_3" $ do
      eval (extend empty "a" 10) (Op (Val 5) Times (Var "a")) `shouldBe` 50
    it "eval_4" $ do
      eval (extend empty "a" 4) (Op (Val 2) Divide (Var "a")) `shouldBe` 0
    it "eval_5" $ do
      eval (extend empty "a" 4) (Op (Val 5) Divide (Var "a")) `shouldBe` 1
    it "eval_6" $ do
      eval (extend empty "a" 5) (Op (Val 5) Gt (Var "a")) `shouldBe` 0
    it "eval_7" $ do
      eval (extend empty "a" 5) (Op (Val 5) Ge (Var "a")) `shouldBe` 1
    it "eval_8" $ do
      eval (extend empty "a" 5) (Op (Val 12) Lt (Var "a")) `shouldBe` 0
    it "eval_9" $ do
      eval (extend empty "a" 13) (Op (Val 12) Le (Var "a")) `shouldBe` 1
    it "eval_10" $ do
      eval (extend empty "a" 5) (Op (Val 5) Eql (Var "a")) `shouldBe` 1
    it "eval_11" $ do
      eval (extend empty "a" 5) (Op (Val 12) Eql (Var "a")) `shouldBe` 0
    it "desugar_1" $ do
      desugar (Assign "x" (Val 5)) `shouldBe` DAssign "x" (Val 5)
    it "desugar_2" $ do
      desugar (Incr "x") `shouldBe` DAssign "x" (Op (Var "x") Plus (Val 1))
    it "runSimpler" $ do
      runSimpler empty (DAssign "x" (Val 5)) "x" `shouldBe` 5
    it "run" $ do
      run empty (Incr "A") "A" `shouldBe` 1
      run
        (extend empty "q" 2)
        ( For
            (Assign "Out" (Val 1))
            (Op (Var "In") Lt (Val 10))
            (Incr "In")
            (Assign "Out" (Op (Var "q") Times (Var "Out")))
        )
        "Out"
        `shouldBe` 1024
      run
        (extend empty "A" 2500)
        ( While
            (Op (Var "A") Ge (Val 3))
            ( Block
                [ Assign "A" (Op (Var "A") Divide (Val 3)),
                  Incr "Out"
                ]
            )
        )
        "Out"
        `shouldBe` 7
    it "state" $ do
      empty "x" `shouldBe` 0
    it "squareRoot" $ do
      run (extend empty "A" 122) squareRoot "B" `shouldBe` 11
    it "fibonacci" $ do
        run (extend empty "In" 9) fibonacci "Out" `shouldBe` 55 
