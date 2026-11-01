-- Не забудьте добавить тесты.
module RootFinding where

import Poly (Poly(..), applyPoly, deriv, nderiv)

-- Поиск корней многочлена ---------------------------------------------
-- Здесь применяются applyPoly и экземпляр Differentiable из модуля Poly.
-- Многочлен p представляет функцию f(t) = applyPoly p t.
-- В заданиях можно использовать Rational или Double; для Double
-- предполагаем, что входные и промежуточные значения конечны (не NaN/Infinity).

-- Задание 1 -----------------------------------------

-- Реализуйте один шаг метода Ньютона:
-- Описание метода: https://en.wikipedia.org/wiki/Newton%27s_method
-- tNext = t - f(t) / f'(t).
-- Если f'(t) == 0, верните Nothing; иначе Just tNext.
-- Например:
-- newtonStep (P [-2, 0, 1] :: Poly Rational) 1 == Just (3 / 2)
-- newtonStep (P [-2, 0, 1] :: Poly Rational) 0 == Nothing
newtonStep :: (Fractional a, Eq a) => Poly a -> a -> Maybe a
newtonStep = undefined

-- Задание 2 -----------------------------------------

-- Реализуйте поиск приближённого корня методом Ньютона.
-- Аргументы: точность epsilon, максимум шагов, многочлен, начальная точка.
-- epsilon задаёт допустимую невязку: abs (f(t)) <= epsilon.
-- Это ограничение на значение многочлена, а не расстояние до точного корня.
-- Если epsilon <= 0 или максимум шагов отрицателен, верните Nothing.
-- Верните Just t для найденного приближения; если лимита шагов недостаточно
-- или продолжению мешает нулевая производная, верните Nothing.
-- Начальная точка может быть ответом даже при лимите 0.
-- Метод может расходиться или зациклиться: лимит гарантирует остановку.
-- Например, после реализации заданий:
-- ghci> fmap (\t -> abs (t - sqrt 2) < 1e-6) $
--         newton 1e-10 20 (P [-2, 0, 1] :: Poly Double) 1
-- Just True
-- ghci> newton 1e-10 0 (P [-2, 1] :: Poly Double) 2
-- Just 2.0
-- ghci> newton 1e-10 10 (P [1, 0, 1] :: Poly Double) 0
-- Nothing
newton :: (Fractional a, Ord a) => a -> Int -> Poly a -> a -> Maybe a
newton = undefined

-- Необязательное задание ----------------------------------------------

-- Реализуйте метод Хэлли, использующий также вторую производную:
-- Описание метода: https://en.wikipedia.org/wiki/Halley%27s_method
-- tNext = t - 2 * f(t) * f'(t) / (2 * f'(t)^2 - f(t) * f''(t)).
-- В halleyStep возвращайте Nothing при нулевом знаменателе.
-- В halley используйте те же правила проверки аргументов, невязки
-- и лимита шагов, что и в newton.
-- Например:
-- halleyStep (P [-2, 0, 1] :: Poly Rational) 1 == Just (7 / 5)
-- Сравните число шагов Ньютона и Хэлли для x^2 - 2 с начальной точкой 1
-- и одинаковой точностью. Всегда ли Хэлли требует меньше шагов?
halleyStep :: (Fractional a, Eq a) => Poly a -> a -> Maybe a
halleyStep = undefined

halley :: (Fractional a, Ord a) => a -> Int -> Poly a -> a -> Maybe a
halley = undefined

-- Можно ли определить такую общую функцию, что newton и halley получаются
-- её применением к newtonStep и halleyStep соответственно?
