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

reverseNumber :: Int -> [Int]
reverseNumber n 
    | ((div n 10) == 0) = [n]
    | otherwise = (mod n 10) : reverseNumber (div n 10) 

doublingEvenNumbers :: [Int] -> [Int]
doublingEvenNumbers [] = []
doublingEvenNumbers [n] = [n] -- doublingEvenNumbers [n] = n : []
doublingEvenNumbers (first:second:end) = first : (mod (2*second) 9) : doublingEvenNumbers end


isLuhnValid :: Int -> Bool
isLuhnValid number = if (mod (foldr (+) 0 (doublingEvenNumbers (reverseNumber number))) 10) == 0 then True else False 