{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lists where

import Data.List (delete)
import Data.Bits (Bits(xor))
-- вектор задаётся списком координат
newtype Point = Point [Double] deriving (Eq, Show, Read)

-- distance x y находит расстояние между двумя точками в n-мерном
-- пространстве. Если число координат точек разное, сообщите об ошибке.
-- distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) == sqrt 2.0
-- distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) == 1.0

-- используйте рекурсию и сопоставление с образцом
distance :: Point -> Point -> Double
-- distance (Point x) (Point y) | length x /= length y = error "Число компонент вектора должно быть одинаковым"
--                             | otherwise = sqrt $ sum [(a-b)^2 |(a, b) <- zip x y]

-- Вспомогательная функция для подсчета суммы квадратов
squareSum :: Point -> Point -> Double
squareSum (Point []) (Point []) = 0
squareSum (Point [x]) (Point [y]) = (x - y)^2
squareSum (Point (x:xs)) (Point (y:ys)) = (x-y)^2 + squareSum (Point xs) (Point ys)

distance (Point x) (Point y) = if length x /= length y then error "Число компонент вектора должно быть одинаковым"
                                                       else  sqrt $ squareSum (Point x) (Point y)

-- intersect xs ys возвращает список, содержащий общие элементы двух списков.
-- intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] == [2, 4] (или [4, 2]!)
-- intersect [1, 2, 4, 6] [3, 5, 7] == []

-- используйте рекурсию и сопоставление с образцом
intersect :: [Integer] -> [Integer] -> [Integer]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) ys
        | x `elem` ys = x : intersect xs (delete x ys)
        | otherwise = intersect xs ys

-- zipN принимает список списков и возвращает список, который состоит из
-- списка их первых элементов, списка их вторых элементов, и так далее.
-- zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
-- zipN [[1, 2, 3], [4, 5], [6]] == [[1, 4, 6], [2, 5], [3]]

firsts :: [[a]] -> [a]
firsts [] = []
firsts ([]:xss) = []
firsts ((x:xs):xss) = x : firsts xss

rests :: [[a]] -> [[a]]
rests [] = []
rests ([]:xss) = []
rests ((x:xs):xss) = xs : rests xss

zipN :: [[a]] -> [[a]]
zipN [] = []
zipN xss | all null xss = []
         | otherwise = firsts xss : zipN (rests xss)

-- Нижеперечисленные функции можно реализовать или рекурсивно, или с помощью
-- стандартных функций для работы со списками (map, filter и т.д.)
-- Попробуйте оба подхода! Хотя бы одну функцию реализуйте обоими способами.

-- Если в списке xs есть такие элементы x, для которых f x == True, то
-- find f xs возвращает Just (первый x), а findLast f xs -- Just (последний x).
-- Если таких нет, то обе функции возвращают Nothing
-- find (> 0) [-1, 2, -3, 4] == Just 2
-- findLast (> 0) [-1, 2, -3, 4] == Just 4
-- find (> 0) [-1, -2, -3] == Nothing
find,findLast :: (a -> Bool) -> [a] -> Maybe a
findFilter :: (a->Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x:xs)
        | f x = Just x
        | otherwise = find f xs

findLast f xs = find f (reverse xs)

findFilter _ [] = Nothing
findFilter f xs = case filter f xs of
        [] -> Nothing
        (x:_) -> Just x

-- mapFuncs принимает список функций fs и возвращает список результатов 
-- применения всех функций из fs к x.
-- mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 == [9, 4, 0]
mapFuncs :: [a -> b] -> a -> [b]
mapFuncs fs x = map (\ f -> f x) fs

-- satisfiesAll принимает список предикатов (функций, возвращающих Bool) preds
-- и возвращает True, если все они выполняются (т.е. возвращают True) для x.
-- Полезные стандартные функции: and, all.
-- satisfiesAll [even, \x -> x rem 5 == 0] 10 == True
-- satisfiesAll [] 4 == True (кстати, почему?)
satisfiesAll :: [a -> Bool] -> a -> Bool
satisfiesAll preds x = all id (mapFuncs preds x)

-- Непустой список состоит из первого элемента (головы)
-- и обычного списка остальных элементов
-- Например, NEL 1 [2, 3] соотвествует списку [1, 2, 3], а NEL 1 [] -- списку [1].
data NEL a = NEL a [a] deriving (Eq, Show, Read)

-- Запишите правильный тип (т.е. такой, чтобы функция имела результат для любых аргументов
-- без вызовов error) и реализуйте функции на NEL, аналогичные tail, last и zip
-- tailNel :: NEL a -> ???
-- lastNel :: NEL a -> ???
-- zipNel :: NEL a -> NEL b -> ???
-- listToNel :: [a] -> ???
-- nelToList :: NEL a -> ???

tailNel :: NEL a -> [a]
tailNel (NEL _ xs) = xs

lastNel :: NEL a -> a
lastNel (NEL x []) = x
lastNel (NEL a (x:xs)) = lastNel (NEL x xs)

zipNel :: NEL a -> NEL b -> NEL (a,b)
zipNel (NEL x xs) (NEL y ys) = NEL (x,y) (zip xs ys)

listToNel :: [a] -> Maybe (NEL a)
listToNel [] = Nothing
listToNel (x:xs) = Just (NEL x xs)

nelToList :: NEL a -> [a]
nelToList (NEL a xs) = a:xs

