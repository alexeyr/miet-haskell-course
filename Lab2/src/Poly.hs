-- Не забудьте добавить тесты.

module Poly where

-- Многочлены
-- a -- тип коэффициентов, список начинается со свободного члена.
-- Бонус: при решении следующих заданий подумайте, какие стали бы проще или
-- сложнее при обратном порядке коэффициентов (и добавьте комментарий).
newtype Poly a = P [a]

-- Задание 1 -----------------------------------------

-- Определите многочлен $x$.
x :: Num a => Poly a
x = undefined

-- Задание 2 -----------------------------------------

-- Функция, считающая значение многочлена в точке
applyPoly :: Num a => Poly a -> a -> a
applyPoly = undefined

-- Задание 3 ----------------------------------------

-- Определите равенство многочленов
-- Заметьте, что многочлены с разными списками коэффициентов
-- могут быть равны! Подумайте, почему.
instance (Num a, Eq a) => Eq (Poly a) where
    (==) = undefined
 
-- Задание 4 -----------------------------------------

-- Найдите степень многочлена: наибольшую степень x с ненулевым коэффициентом.
-- Для нулевого многочлена верните Nothing.
-- Например:
-- degree (P [])              == Nothing
-- degree (P [0, 0])          == Nothing
-- degree (P [5])             == Just 0
-- degree (P [1, 0, 3, 0, 0]) == Just 2
degree :: (Num a, Eq a) => Poly a -> Maybe Int
degree = undefined

-- Задание 5 -----------------------------------------

-- Определите сложение многочленов
plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Задание 6 -----------------------------------------

-- Определите умножение многочленов
times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Задание 7 -----------------------------------------

-- Сделайте многочлены числовым типом
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- Эти функции оставить как undefined, поскольку для 
    -- многочленов они не имеют математического смысла
    abs    = undefined
    signum = undefined

-- Задание 8 -----------------------------------------

-- Определите композицию многочленов: composePoly p q представляет p(q(x)).
-- Можно использовать уже реализованные арифметические операции.
-- Например:
-- composePoly (P [1, 0, 1]) (P [1, 1]) == P [2, 2, 1]
-- Порядок аргументов важен:
-- applyPoly (composePoly p q) t == applyPoly p (applyPoly q t)
composePoly :: Num a => Poly a -> Poly a -> Poly a
composePoly = undefined

-- Задание 9 -----------------------------------------

-- Реализуйте nderiv через deriv
class Num a => Differentiable a where
    -- взятие производной
    deriv  :: a -> a
    -- взятие n-ной производной
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Задание 10 ----------------------------------------

-- Определите экземпляр класса типов
instance Num a => Differentiable (Poly a) where
    deriv = undefined

-- Готовый вывод многочленов --------------------------------------------
-- Для записи со знаками + и - используем упорядоченные коэффициенты.
-- Реализация не зависит от функций, которые нужно написать в заданиях.
-- Например: show (P [1, -1, 3] :: Poly Integer) == "3 * x^2 - x + 1".
instance (Num a, Ord a, Show a) => Show (Poly a) where
    showsPrec precedence (P coefficients) =
        case terms of
            [] -> showString "0"
            [(0, coefficient)] -> showsPrec precedence coefficient
            first : rest ->
                showParen (precedence > expressionPrecedence first rest) $
                    showString (firstTerm first ++ concatMap nextTerm rest)
      where
        terms = [(power, coefficient)
                | (power, coefficient) <- reverse (zip [0 :: Int ..] coefficients)
                , coefficient /= 0]

        firstTerm term@(_, coefficient) =
            (if coefficient < 0 then "-" else "") ++ termBody term

        nextTerm term@(_, coefficient) =
            (if coefficient < 0 then " - " else " + ") ++ termBody term

        termBody (0, coefficient) = magnitudeText 0 coefficient
        termBody (power, coefficient)
            | coefficient == 1 || coefficient == -1 = powerText power
            | otherwise = magnitudeText 8 coefficient ++ " * " ++ powerText power

        powerText 1 = "x"
        powerText power = "x^" ++ show power

        magnitudeText context coefficient
            -- Отрицание minBound :: Int тоже переполняется.
            -- В этом случае убираем минус из записи, не вычисляя модуль.
            | magnitude < 0 = case show magnitude of
                '-' : digits -> digits
                text -> text
            | otherwise = showsPrec context magnitude ""
          where
            magnitude = if coefficient < 0 then negate coefficient else coefficient

        expressionPrecedence (_, _) (_ : _) = 6
        expressionPrecedence (power, coefficient) []
            | coefficient < 0 = 6
            | coefficient == 1 && power == 1 = 11
            | coefficient == 1 = 8
            | otherwise = 7
