import Poly
import RootFinding
import SimpleLang
import Test.Hspec

-- Это только начальная заготовка: добавьте собственные тесты,
-- в том числе для граничных случаев и отсутствия сходимости.

main :: IO ()
main = hspec $ do
    describe "poly" $ do
        it "applyPoly" $ pending
        it "degree" $ pending
        it "composePoly" $ pending
        describe "Eq" $ do
            it "(==)" $ pending
            it "пустой список и список нулей задают нулевой многочлен" $ pending
            it "нули в конце списка не меняют многочлен" $ pending
        describe "Num" $ do
            it "(+)" $ pending
            it "(*)" $ pending
            it "negate" $ pending
            it "fromInteger" $ pending
            it "складывает многочлены с разным числом коэффициентов" $ pending
            it "умножение на нулевой многочлен даёт нулевой многочлен" $ pending
        describe "Differentiable" $ do
            it "apply совпадает с applyPoly" $ pending
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
    describe "rootFinding" $ do
        describe "newtonStep" $ do
            it "делает один шаг для x^2 - 2" $ do
                pending
                newtonStep (P [-2, 0, 1] :: Poly Rational) 1
                    `shouldBe` Just (3 / 2)
            it "возвращает Nothing при нулевой производной" $ pending
        describe "newton" $ do
            it "находит приближение к sqrt 2 с заданной невязкой" $ pending
            it "принимает начальный корень даже при лимите 0" $ pending
            it "проверяет последнюю точку после разрешённого числа шагов" $ pending
            it "возвращает Nothing при исчерпании лимита" $ pending
            it "возвращает Nothing при нулевой производной вне корня" $ pending
            it "отклоняет неположительную точность и отрицательный лимит" $ pending
        describe "halley (необязательно)" $ do
            it "halleyStep делает один шаг для x^2 - 2" $ pending
            it "возвращает Nothing при нулевом знаменателе вне корня" $ pending
            it "находит приближение к sqrt 2" $ pending
            it "соблюдает те же правила точности и лимита, что и newton" $ pending
    describe "simpleLang" $ do
        -- Замените pending своими тестами. В примере factorial достаточно
        -- удалить pending, когда будут реализованы нужные функции.
        -- Состояния — функции: сравнивайте значения выбранных переменных.
        describe "empty и extend" $ do
            it "empty возвращает 0 для любой переменной" $ pending
            it "extend меняет выбранную переменную, сохраняя остальные" $ pending
            it "повторный extend заменяет предыдущее значение" $ pending
        describe "eval" $ do
            it "вычисляет арифметику и сравнения (1 или 0)" $ pending
            it "деление отрицательных чисел округляет вниз" $ pending
            it "деление на ноль вызывает ошибку" $ pending
        describe "desugar" $ do
            it "преобразует Incr в присваивание" $ pending
            it "преобразует For с порядком initialization, condition, body, step" $ pending
            it "преобразует Block [] в DSkip" $ pending
        describe "runSimpler и run" $ do
            it "пустой блок сохраняет значения переменных" $ pending
            it "цикл с ложным начальным условием не выполняет тело" $ pending
            it "For выполняет инициализацию даже при ложном начальном условии" $ pending
            it "factorial записывает результат в Out" $ do
                pending
                let st = run (extend empty "In" 5) factorial
                st "Out" `shouldBe` 120
            it "factorial для 0 возвращает 1" $ pending
            it "squareRoot обрабатывает 0, точные квадраты и числа между ними" $ pending
            it "fibonacci возвращает 1 для In = 0 и In = 1" $ pending
