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


isLuhnValid :: Int -> Bool

digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]
-- print digits


processDigits :: [Int] -> [Int]
processDigits = map processDigit . zip [0..] . reverse
where
processDigit (i, d)
| even i = doubleDigit d
| otherwise = d

doubleDigit :: Int -> Int
doubleDigit d = let doubled = 2 * d in if doubled > 9 then doubled - 9 else doubled


isLuhnValid ds = sum (processDigits ds) `mod` 10 == 0









-- Тесты
testDigits :: Bool
testDigits = digits 12345 == [5,4,3,2,1]

testProcess :: Bool
testProcess = processDigits [5,4,3,2,1] == [5,8,3,4,1]

testLuhn :: Bool
testLuhn = and [
    isLuhnValid 79927398713,    
    not (isLuhnValid 7992739871),
    isLuhnValid 0,
    not (isLuhnValid 1)
    ]

