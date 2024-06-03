module Lists where

-- вектор задаётся списком координат
data Point = Point {coords :: [Double]} deriving (Eq, Show, Read)

--{coords :: [Double]}

-- distance x y находит расстояние между двумя точками в n-мерном
-- пространстве. Если число координат точек разное, сообщите об ошибке.
-- distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) == sqrt 2.0
-- distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) == 1.0

-- используйте рекурсию и сопоставление с образцом

distance, abnormDistance :: Point -> Point -> Double

abnormDistance p1 p2 
    | not (length (coords p1) == length (coords p2)) = error "DifferentDimensionError"
    | (null (coords p1) && null (coords p2)) = 0
    | otherwise = ((head (coords p2)) - (head (coords p1)))^2 + abnormDistance (Point(tail (coords p1))) (Point(tail (coords p2)))

distance fstCoord sndCoords = sqrt $ (abnormDistance fstCoord sndCoords)

-- intersect xs ys возвращает список, содержащий общие элементы двух списков.
-- intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] == [2, 4] (или [4, 2]!)
-- intersect [1, 2, 4, 6] [3, 5, 7] == []

-- используйте рекурсию и сопоставление с образцом
intersect :: [Integer] -> [Integer] -> [Integer]
--intersect [] _ =  []
--intersect _  [] =  []
-- intersect xs ys = [x | x <- xs, any ((==) x) ys]
intersect xs ys | (null xs || null ys) = []
                | otherwise  = [x | x <- xs, any ((==) x) ys]

-- zipN принимает список списков и возвращает список, который состоит из
-- списка их первых элементов, списка их вторых элементов, и так далее.
-- zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]

zipN :: (Num a) => [[a]] -> [[a]]
zipN ((x:xs):xss) = (x : zipN' xss) : zipN (helper xs xss) where
  zipN' ((a:as):ass) = a : zipN' ass 
  zipN' _ = [] 
  helper [] ((b:bs):bss) = bs : helper [] bss
  helper b_b ((b:bs):bss) = b_b : (bs : helper [] bss)
  helper _ _ = []  
zipN _ = []


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

{- find f [] = Nothing
find f xs = Just (head (filter f xs)) -}

find f [] = Nothing
find f xs
    | (f (head xs)) = Just (head xs)
    | not (f (head xs)) && (not (null (tail xs))) = find f (tail xs)
    | null (tail xs) = Nothing

findLast f x | (length (filter f x)) > 0 = Just $ last (filter f x)
             | otherwise = Nothing

-- mapFuncs принимает список функций fs и возвращает список результатов 
-- применения всех функций из fs к x.
-- mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 == [9, 4, 0]
mapFuncs :: [a -> b] -> a -> [b]
mapFuncs [] _ = []
mapFuncs fs x = [(f x) | f <- fs]

-- satisfiesAll принимает список предикатов (функций, возвращающих Bool) preds 
-- и возвращает True, если все они выполняются 
-- (т.е. возвращают True) для x. Полезные стандартные функции: and, all.
-- satisfiesAll [even, \x -> x rem 5 == 0] 10 == True
-- satisfiesAll [] 4 == True (кстати, почему?). Потому что в пустом списке нет предикатов, возвращающих True, а значит ни один предикат не будет применён к аргументам
satisfiesAll :: [a -> Bool] -> a -> Bool
satisfiesAll [] _ = True
satisfiesAll preds x
    | all (True ==) (mapFuncs preds x) = True
    | any (False ==) (mapFuncs preds x) = False
    | otherwise = False

-- непустой список состоит из первого элемента (головы)
-- и обычного списка остальных элементов
data NEL a = NEL a [a] deriving (Eq, Show, Read)

-- запишите правильный тип (т.е. такой, чтобы функция имела результат для любых аргументов)
-- и реализуйте функции на NEL, аналогичные tail, last и zip
-- tailNel :: NEL a -> ???
-- lastNel :: NEL a -> ???
-- zipNel :: NEL a -> NEL b -> ???
-- listToNel :: [a] -> ???
-- nelToList :: NEL a -> ???

tailNel :: NEL a -> [a]
tailNel (NEL x xs) = xs

lastNel :: NEL a -> a
lastNel (NEL x xs)
    | null xs = x
    | (not (null xs)) = last xs

listToNel :: [a] -> NEL a 
listToNel (x:xs) = NEL x xs 

nelToList :: NEL a -> [a]
nelToList (NEL x xs) = x : xs


zipNel :: NEL a -> NEL b -> NEL (a,b)
zipNel a b = listToNel $ zip (nelToList a) (nelToList b) 
 