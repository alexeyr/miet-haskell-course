module Luhn where

-- Проверка корректности номера банковской карты алгоритмом Луна https://ru.wikipedia.org/wiki/Алгоритм_Луна.
-- Алгоритм:
-- 1. Все цифры, стоящие на чётных местах (считая с конца), удваиваются. Если при этом получается число, большее 9, то из него вычитается 9. Цифры, стояшие на нечётных местах, не изменяются.
-- То есть: последняя цифра не меняется; предпоследнее удваивается; 3-е с конца (предпредпоследнее) не меняется; 4-е с конца удваивается и т.д.
-- 2. Все полученные числа складываются.
-- 3. Если полученная сумма кратна 10, то исходный список корректен.

-- Не пытайтесь собрать всё в одну функцию, используйте вспомогательные.
-- Например: разбить число на цифры (возможно, сразу в обратном порядке).
-- Не забудьте добавить тесты, в том числе для вспомогательных функций!
-- intToList 0 == []
-- intToList 1 == [1]
-- intToList 123 == [321] (returns reversed list)
intToList :: Int -> [Int]
intToList 0 = []
intToList x = [x `mod` 10] ++ intToList (x `div` 10)


-- processEven [] == []
-- processEven [1] == [1]
-- processEven [1, 1] == [1, 2]
-- processEven [1, 1, 1] == [1, 2, 1]
processEven :: [Int] -> [Int]
processEven digs = [(digs !! i) * (2 ^ (i `mod` 2))| i <- [0..(length digs) - 1]]

-- decreaseLarge 1 == 1
-- decreaseLarge 9 == 9
-- decreaseLarge 10 == 1
-- decreaseLarge 18 == 9
decreaseLarge :: Int -> Int
decreaseLarge x
  | x > 9 = x - 9
  | otherwise = x

-- decreaseAllLarge [] == []
-- decreaseAllLarge [1] == [1]
-- decreaseAllLarge [1, 9, 3] == [1, 2, 3]
-- decreaseAllLarge [10, 2, 18] == [1, 2, 9]
decreaseAllLarge :: [Int] -> [Int]
decreaseAllLarge digs = map decreaseLarge digs

isSumDivisible :: [Int] -> Bool
isSumDivisible [] = False
isSumDivisible digs = ((sum digs) `mod` 10) == 0

-- isLuhnValid 0 == False
-- isLuhnValid 1241235125 == False
-- isLuhnValid 4140834708961565 == True (generated with https://randommer.io/Card)
isLuhnValid :: Int -> Bool
isLuhnValid x = isSumDivisible digs1
  where digs1 = decreaseAllLarge digs2
                where digs2 = processEven digs3
                              where digs3 = intToList x
