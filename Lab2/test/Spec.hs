import Poly
import SimpleLang
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "poly" $ do
        it "applyPoly evaluates polynomial correctly" $ do
            applyPoly (P [1, 2, 3]) 2 `shouldBe` 17
            applyPoly (P [0, 0, 1]) 3 `shouldBe` 9
            applyPoly (P [5]) 10 `shouldBe` 5

        it "plus adds two polynomials correctly" $ do
            plus (P [1, 2, 3]) (P [4, 5, 6]) `shouldBe` P [5, 7, 9]
            plus (P [1, 2]) (P [3, 4, 5]) `shouldBe` P [4, 6, 5]
            plus (P [0]) (P [0]) `shouldBe` P [0]

        it "times multiplies two polynomials correctly" $ do
            times (P [1, 2]) (P [3, 4]) `shouldBe` P [3, 10, 8]
            times (P [1, 0, 2]) (P [2, 1]) `shouldBe` P [2, 1, 4, 2]
            times (P [0]) (P [1, 2, 3]) `shouldBe` P [0]

        it "deriv calculates the derivative of a polynomial correctly" $ do
            deriv (P [1, 2, 3]) `shouldBe` P [2, 6]
            deriv (P [0, 0, 1]) `shouldBe` P [0, 2]
            deriv (P [5]) `shouldBe` P []

        it "nderiv calculates the n-th derivative of a polynomial correctly" $ do
            nderiv 1 (P [1, 2, 3]) `shouldBe` P [2, 6]
            nderiv 2 (P [1, 2, 3]) `shouldBe` P [6]
            nderiv 3 (P [1, 2, 3]) `shouldBe` P []
            nderiv 0 (P [1, 2, 3]) `shouldBe` P [1, 2, 3]
    describe "simpleLang" $ do
        it "desugar converts Incr, For, Block to DietStatement" $ do
        let stmt = Block [Assign "X" (Val 1), Incr "X"]
            desugared = desugar stmt
            expected = DSequence (DAssign "X" (Val 1)) (DAssign "X" (Op (Var "X") Plus (Val 1)))
        desugared `shouldBe` expected

        it "eval computes arithmetic and comparisons correctly" $ do
        let st = extend empty "X" 3
        eval st (Op (Var "X") Plus (Val 2)) `shouldBe` 5
        eval st (Op (Var "X") Times (Val 4)) `shouldBe` 12
        eval st (Op (Var "X") Gt (Val 2)) `shouldBe` 1
        eval st (Op (Var "X") Eql (Val 3)) `shouldBe` 1
        eval st (Op (Var "X") Lt (Val 0)) `shouldBe` 0

        it "runSimpler executes DietStatement correctly" $ do
        let stmt = DSequence (DAssign "X" (Val 1)) (DAssign "Y" (Op (Var "X") Plus (Val 2)))
            st' = runSimpler empty stmt
        st' "X" `shouldBe` 1
        st' "Y" `shouldBe` 3

        it "run executes Simple program correctly" $ do
        let stmt = Block [Assign "X" (Val 1), Incr "X"]
            st' = run empty stmt
        st' "X" `shouldBe` 2

        it "factorial of 5" $ do
        let st0 = extend empty "In" 5
            stF = run st0 factorial
        stF "Out" `shouldBe` 120

        it "square root of 10" $ do
        let st0 = extend empty "A" 10
            stS = run st0 squareRoot
        stS "B" `shouldBe` 3

        it "fibonacci numbers" $ do
        let fib n = run (extend empty "In" n) fibonacci
        fib 0 "Out" `shouldBe` 1
        fib 1 "Out" `shouldBe` 1
        fib 5 "Out" `shouldBe` 8
        fib 10 "Out" `shouldBe` 89
