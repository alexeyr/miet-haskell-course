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
x = P [0, 1]

-- Задание 2 -----------------------------------------

-- Функция, считающая значение многочлена в точке
applyPoly :: Num a => Poly a -> a -> a
-- не без помощи интрнета, но нашел достаточно простую 
-- и красивую реализацию по схеме Горнера 
-- P(x) = a0 + x * (a1 + x * (a2 + x * (a3 + ... + x * an)...))
applyPoly (P coeffs) x = gorner coeffs x
where 
gorner [] _ = 0
gorner (c:cs) x = c + x * gorner cs x 
-- рекурсивно каждый раз забираем один коэффиуиент 

-- Задание 3 ----------------------------------------

-- Определите равенство многочленов
-- Заметьте, что многочлены с разными списками коэффициентов
-- могут быть равны! Подумайте, почему.
instance (Num a, Eq a) => Eq (Poly a) where
    (P1 cs1) (==) (P2 cs 2) = filter_coeffs cs1 == filter_coeffs cs2
    where 
    filter_coeffs cs = takeWhile (/= zero) (pad cs)
    zero = 0 -- воркэраунд чтобы сравнивать с одним типом
    pad cs = cs ++ repeat zero
 
-- Задание 4 -----------------------------------------

-- Определите перевод многочлена в строку. 
-- Это должна быть стандартная математическая запись, 
-- например: show (3 * x * x + 1) == "3 * x^2 + 1").
-- (* и + для многочленов можно будет использовать после задания 6.)
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show = undefined

-- Задание 5 -----------------------------------------

-- Определите сложение многочленов
plus :: Num a => Poly a -> Poly a -> Poly a

plus (P xs) (P ys) = P (add xs ys)
where
add [] bs = bs
add as [] = as
add (a:as) (b:bs) = (a + b) : add as bs


-- Задание 6 -----------------------------------------

-- Определите умножение многочленов
times :: Num a => Poly a -> Poly a -> Poly a
times (P xs) (P ys) = foldr plus (P []) terms
  where
    terms =
      [ P (replicate i 0 ++ map (a *) ys)
      | (a, i) <- zip xs [0..]
      ]


-- Задание 7 -----------------------------------------

-- Сделайте многочлены числовым типом
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P xs) = P (map negate xs)
    fromInteger n = P [fromInteger n]   
    -- Эти функции оставить как undefined, поскольку для 
    -- многочленов они не имеют математического смысла
    abs    = undefined
    signum = undefined

-- Задание 8 -----------------------------------------

-- Реализуйте nderiv через deriv
class Num a => Differentiable a where
    -- взятие производной
    deriv  :: a -> a
    -- взятие n-ной производной
    nderiv :: Int -> a -> a
    nderiv n x
      | n <= 0    = x
      | otherwise = nderiv (n - 1) (deriv x)
-- Задание 9 -----------------------------------------

-- Определите экземпляр класса типов
instance Num a => Differentiable (Poly a) where
    deriv (P (_:xs)) = P (zipWith (*) xs [1..])
    deriv (P [])     = P []

