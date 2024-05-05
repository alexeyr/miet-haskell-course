-- Не забудьте добавить тесты.

module Poly where

import Data.List (intercalate) -- для п.4

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
applyPoly (P []) _ = 0
applyPoly (P lst) x = (head lst) + x * applyPoly (P (tail lst)) x


-- Задание 3 ----------------------------------------

-- Определите равенство многочленов
-- Заметьте, что многочлены с разными списками коэффициентов
-- могут быть равны! Подумайте, почему - список можно дополнять нулями для членов полинома с большей степенью

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = checkNull a == checkNull b where
        checkNull [] = [] 
        checkNull lst = if (last lst == 0) then checkNull (init lst) else lst 

 
-- Задание 4 -----------------------------------------

-- Определите перевод многочлена в строку. 
-- Это должна быть стандартная математическая запись, 
-- например: show (3 * x * x + 1) == "3 * x^2 + 1").
-- (* и + для многочленов можно будет использовать после задания 6.) -- возникли трудности с решением (прим.)

showPoly [] = show 0 
showPoly p =  let cOs = zip p [0..]
                  nonZeroCOs = filter (\(c,_) -> c /= 0) cOs
                  cShow c = if c == 1 then "" else show c ++ " *"
                  nShow n = case n of 
                              0 -> ""
                              1 -> "x" 
                              m -> "x^" ++ show m
                  cnShow c n = if c == 1 && n == 0 then show 1 
                               else intercalate " " $ filter (/="") [cShow c, nShow n]            
                  terms = map (\(c,n) -> cnShow c n) nonZeroCOs
              in intercalate " + " (reverse terms) 


instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = showPoly p

-- Задание 5 -----------------------------------------

-- Определите сложение многочленов

listCalc :: Num a => [a] -> [a] -> [a]-- обработка списков 
listCalc [] a = a
listCalc a [] = a
listCalc a b = head a + head b : listCalc (tail a) (tail b) -- рекурсивное сложение элементов


plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P (listCalc a b) 


-- Задание 6 -----------------------------------------

-- Определите умножение многочленов

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = P (multLists a b) where
    multLists [] _ = [0] 
    multLists _ [] = [0]  
    multLists lst1 lst2 = listCalc (multScalar (head lst1) lst2) (0 : multLists (tail lst1) lst2)
        where
            multScalar :: Num a => a -> [a] -> [a]
            multScalar _ [] = []
            multScalar x lstV = x * (head lstV) : multScalar x (tail lstV)


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
    nderiv 0 a = a
    nderiv n a = nderiv (n - 1) (deriv a)


-- Задание 9 -----------------------------------------

-- Определите экземпляр класса типов

listDif :: Num a => [a] -> a -> [a] -- взятие первой производной от полинома
listDif [] _ = []
listDif lst n = head lst * n : listDif (tail lst) (n + 1)



instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P poly) = P (listDif (tail poly) 1)