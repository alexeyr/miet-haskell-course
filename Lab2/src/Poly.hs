-- Не забудьте добавить тесты.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Poly where
import Data.List (dropWhileEnd, intercalate)

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
applyPoly (P coef) value = sum $ zipWith (\coeffs power -> coeffs * (value ^ power)) coef [0..]

-- Задание 3 ----------------------------------------

-- Определите равенство многочленов
-- Заметьте, что многочлены с разными списками коэффициентов
-- могут быть равны! Подумайте, почему.

normalize :: (Num a, Eq a) => [a] -> [a]
normalize = dropWhileEnd (== 0)

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P xs) (P ys) = normalize xs == normalize ys

-- Задание 4 -----------------------------------------

-- Определите перевод многочлена в строку. 
-- Это должна быть стандартная математическая запись, 
-- например: show (3 * x * x + 1) == "3 * x^2 + 1").
-- (* и + для многочленов можно будет использовать после задания 6.)
-- instance (Num a, Eq a, Show a, Ord a) => Show (Poly a) where
--     show (P coefs) = intercalate " " (zipWith showTerm (reverse coefs) (reverse [0..length coefs-1]))
--         where 
--             showTerm :: (Num a, Eq a, Show a, Ord a) => a -> Int -> String
--             showTerm coeff power 
--                 | coeff == 0 = ""
--                 | coeff == 1 && power > 0 = "x^" ++ show(power) ++ " "
--                 | coeff == -1 && power > 0 = "-x^" ++ show(power) ++ " "
--                 | power == 0 = show coeff
--                 | power == 1 = show coeff ++ "x" ++ " "
--                 | otherwise = if coeff > 0 then " + " ++ show coeff ++ "x^" ++ show(power) else " - " ++show (-1*coeff) ++ "x^" ++ show(power)

instance (Num a, Eq a, Show a, Ord a) => Show (Poly a) where
    show (P coeffs) = intercalate "" (filter (not . null) terms)
        where
            coeffss =  normalize coeffs
            terms = zipWith showTerm (reverse coeffss) (reverse [0..length coeffss - 1])
            showTerm :: (Num a, Eq a, Show a, Ord a) => a -> Int -> String
            showTerm coeff power
                | power == length coeffss - 1 = show (coeff) ++ termPower 
                | coeff == 0              = ""
                | coeff < 0               = " - " ++ show (abs coeff) ++ termPower
                | coeff == 1 && power > 0 = termPower
                | otherwise               = " + " ++ show coeff ++ termPower
                where termPower
                        | power == 0 = ""
                        | power == 1 = "x"
                        | otherwise = "x^" ++ show (power)

-- Задание 5 -----------------------------------------

-- Определите сложение многочленов
addLists :: Num a => [a] -> [a] -> [a]
addLists [] xs = xs
addLists xs [] = xs
addLists (c:cs) (y:ys) = c + y : addLists cs ys

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P (addLists a b)

-- Задание 6 -----------------------------------------

-- Определите умножение многочленов
times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = P (multiplyPoly a b)

-- Функция для умножения двух многочленов
multiplyPoly :: Num a => [a] -> [a] -> [a]
multiplyPoly [] _ = [0] 
multiplyPoly _ [] = [0]  
multiplyPoly (x:xs) ys = addLists (multiplyByConst x ys) (0 : multiplyPoly xs ys)

-- Функция для умножения многочлена на константу
multiplyByConst :: Num a => a -> [a] -> [a]
multiplyByConst _ [] = []
multiplyByConst c (y:ys) = c * y : multiplyByConst c ys

-- Задание 7 -----------------------------------------

-- Сделайте многочлены числовым типом
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate  (P coeffs)   = P (map negate coeffs)
    fromInteger a = P [fromInteger a]
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
    nderiv 0 p = p
    nderiv n p = nderiv (n - 1) (deriv p)

-- Задание 9 -----------------------------------------

-- Определите экземпляр класса типов
instance Num a => Differentiable (Poly a) where
    deriv (P [])     = P []
    deriv (P (_:xs)) = P $ zipWith (*) (map fromInteger [1..]) xs

