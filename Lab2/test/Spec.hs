import Poly
import SimpleLang
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "poly" $ do
        it "applyPoly" $ pending
        it "degree" $ pending
        it "composePoly" $ pending
        describe "Eq" $ do
            it "(==)" $ pending
        describe "Num" $ do
            it "(+)" $ pending
            it "(*)" $ pending
            it "negate" $ pending
            it "fromInteger" $ pending
        describe "Differentiable" $ do
            it "deriv" $ pending
            it "nderiv" $ pending
        describe "show (готовая реализация)" $ do
            let render coefficients = show (P coefficients :: Poly Integer)
            it "выводит нулевой многочлен как 0" $ do
                render [] `shouldBe` "0"
                render [0, 0, 0] `shouldBe` "0"
            it "выводит константы" $ do
                render [5] `shouldBe` "5"
                render [-5] `shouldBe` "-5"
            it "опускает нулевые члены и лишние нули в конце" $ do
                render [1, 0, 3, 0, 0] `shouldBe` "3 * x^2 + 1"
            it "опускает коэффициенты 1 и -1 при степенях x" $ do
                render [0, 1] `shouldBe` "x"
                render [0, -1] `shouldBe` "-x"
                render [0, 0, 1] `shouldBe` "x^2"
                render [0, 0, -1] `shouldBe` "-x^2"
            it "выводит члены по убыванию степени с обычными знаками" $ do
                render [1, -1, 3] `shouldBe` "3 * x^2 - x + 1"
                render [-1, 2, -3] `shouldBe` "-3 * x^2 + 2 * x - 1"
            it "поддерживает дробные коэффициенты" $ do
                show (P [0.5, -1.5] :: Poly Double) `shouldBe` "-1.5 * x + 0.5"
                show (P [1 / 2, -3 / 2] :: Poly Rational)
                    `shouldBe` "-(3 % 2) * x + 1 % 2"
            it "не переполняется при выводе минимального Int" $ do
                let smallest = minBound :: Int
                    magnitude = show (negate (toInteger smallest))
                show (P [smallest] :: Poly Int) `shouldBe` show smallest
                show (P [1, smallest] :: Poly Int)
                    `shouldBe` "-" ++ magnitude ++ " * x + 1"
            it "учитывает приоритет при вложенном выводе" $ do
                show (Just (P [1, 1] :: Poly Integer)) `shouldBe` "Just (x + 1)"
                show (Just (P [0, 1] :: Poly Integer)) `shouldBe` "Just x"
                show (Just (P [0, 0, 1] :: Poly Integer)) `shouldBe` "Just (x^2)"
    describe "simpleLang" $ do
        -- включите тесты на работу 
        it "desugar" $ pending
