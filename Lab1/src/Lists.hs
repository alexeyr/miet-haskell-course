module Lists where

-- removeFirst x xs удаляет первое вхождение x из списка xs.
-- Если x не встречается в списке, список остаётся без изменений.
-- Порядок остальных элементов сохраняется.
-- removeFirst 2 [1, 2, 3, 2] == [1, 3, 2]
-- removeFirst 4 [1, 2, 3] == [1, 2, 3]
-- removeFirst 2 [] == []

-- используйте рекурсию и сопоставление с образцом
removeFirst :: Integer -> [Integer] -> [Integer]
removeFirst x xs = error "todo"

-- interleave xs ys чередует элементы списков xs и ys, начиная с xs.
-- Когда один список заканчивается, добавьте остаток другого списка.
-- interleave [1, 2, 3] [10, 20, 30] == [1, 10, 2, 20, 3, 30]
-- interleave [1, 2, 3] [10] == [1, 10, 2, 3]
-- interleave [] [10, 20] == [10, 20]
-- interleave [1, 2] [] == [1, 2]

-- используйте рекурсию и сопоставление с образцом
interleave :: [a] -> [a] -> [a]
interleave xs ys = error "todo"

-- вектор задаётся списком координат
newtype Point = Point [Double] deriving (Eq, Show, Read)

-- distance x y находит расстояние между двумя точками в n-мерном
-- пространстве. Если число координат точек разное, сообщите об ошибке.
-- distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) == sqrt 2.0
-- distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) == 1.0

-- используйте рекурсию и сопоставление с образцом
distance :: Point -> Point -> Double
distance x y = error "todo"

-- Нижеперечисленные функции можно реализовать или рекурсивно, или с помощью
-- стандартных функций для работы со списками (map, filter и т.д.)
-- Попробуйте оба подхода! Хотя бы одну функцию реализуйте обоими способами.

-- Если в списке xs есть такие элементы x, для которых f x == True, то
-- find f xs возвращает Just (первый x), а findLast f xs -- Just (последний x).
-- Если таких нет, то обе функции возвращают Nothing
-- find (> 0) [-1, 2, -3, 4] == Just 2
-- findLast (> 0) [-1, 2, -3, 4] == Just 4
-- find (> 0) [-1, -2, -3] == Nothing
find, findLast :: (a -> Bool) -> [a] -> Maybe a
find f xs = error "todo"
findLast f xs = error "todo"

-- mapFuncs принимает список функций fs и возвращает список результатов 
-- применения всех функций из fs к x.
-- mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 == [9, 4, 0]
mapFuncs :: [a -> b] -> a -> [b]
mapFuncs fs x = error "todo"

-- satisfiesAll принимает список предикатов (функций, возвращающих Bool) preds
-- и возвращает True, если все они выполняются (т.е. возвращают True) для x.
-- Полезные стандартные функции: and, all.
-- satisfiesAll [even, \x -> x `rem` 5 == 0] 10 == True
-- satisfiesAll [] 4 == True (кстати, почему?)
satisfiesAll :: [a -> Bool] -> a -> Bool
satisfiesAll preds x = error "todo"

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
