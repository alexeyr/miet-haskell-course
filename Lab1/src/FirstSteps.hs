module FirstSteps 
where
import Data.Word (Word8)

-- xor x y находит "исключающее или" x и y
-- xor True False == True
-- xor True True == False

-- используйте сопоставление с образцом
xor :: Bool -> Bool -> Bool
xor x y = if x == y then False else True

-- max3 x y z находит максимум из x, y и z
-- max3 1 3 2 == 3
-- max3 5 2 5 == 5
-- median3 x y z находит второе по величине число (медиану)
-- median3 1 3 2 == 2
-- median3 5 2 5 == 5
max3, median3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = max (max x y) z  -- if z >= a then z else a where {a = x >= y then z else y} 
median3 x y z | (min x z <= y) && (y <= max x z) = y --sum
              | (min y z <= x) && (x <= max y z) = x
              | (min x y <= z) && (z <= max x y) = z
              | otherwise = error "the median has not been calculated"

-- Типы данных, описывающие цвета в моделях 
-- RGB (https://ru.wikipedia.org/wiki/RGB), компоненты от 0 до 255
-- и CMYK (https://ru.wikipedia.org/wiki/CMYK), компоненты от 0.0 до 1.0
data RGB = RGB { red :: Word8, green :: Word8, blue :: Word8 } deriving (Eq, Show, Read)
data CMYK = CMYK { cyan :: Double, magenta :: Double, yellow :: Double, black :: Double } deriving (Eq, Show, Read)
-- Задайте функцию для их преобразования
-- (формулы из http://www.codeproject.com/Articles/4488/XCmyk-CMYK-to-RGB-Calculator-with-source-code):
-- Black   = min(1-Red, 1-Green, 1-Blue)
-- Cyan    = (1-Red-Black) / (1-Black)
-- Magenta = (1-Green-Black) / (1-Black)
-- Yellow  = (1-Blue-Black) / (1-Black) 
-- где значения Red, Green и Blue нормализованы от 0 до 1).

-- Заметьте, что (/) для Int не работает, и неявного преобразования Int в Double нет.
-- Это преобразование производится с помощью функции fromIntegral.
rbgToCmyk :: RGB -> CMYK
rbgToCmyk (RGB 0 0 0) = CMYK 0.0 0.0 0.0 1.0
rbgToCmyk color = cmyk_color where
    r = fromIntegral(red color) / 255.0
    g = fromIntegral(green color) / 255.0
    b = fromIntegral(blue color) / 255.0
    k = (min (min (1.0 - r)  (1.0 - g)) (1.0 - b))
    c = (1.0 - r - k) / (1.0 - k)
    m = (1.0 - g - k) / (1.0 - k)
    y = (1.0 - b - k) / (1.0 - k)
    cmyk_color = CMYK c m y k


-- geomProgression b q n находит n-й (считая с 0) член 
-- геометрической прогрессии, нулевой член которой -- b, 
-- а знаменатель -- q.
-- geomProgression 3.0 2.0 2 == 12.0

-- используйте рекурсию
-- не забудьте случаи n < 0 и n == 0.
geomProgression :: Double -> Double -> Integer -> Double
geomProgression b q n | (n == 0) = b 
                      | n < 0 = error "n must be a natural number: n >= 2"
                      | n > 0 = q * geomProgression b q (n - 1)  

-- coprime a b определяет, являются ли a и b взаимно простыми
-- (определение: Целые числа называются взаимно простыми, 
-- если они не имеют никаких общих делителей, кроме +/-1)
-- coprime 10 15 == False
-- coprime 12 35 == True

-- Используйте рекурсию
-- Есть ли важные пограничные случаи или вспомогательные функции? Не забудьте добавить их в тесты.

-- Полезные функции в Prelude (автоматически загруженной
-- части стандартной библиотеки): quot, rem, quotRem 
-- (или div, mod, divMod в зависимости от того, как 
-- обрабатываете отрицательные числа)
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html
{- euclidAlgorithm :: Integer -> Integer -> Integer
euclidAlgorithm x 0 = x
euclidAlgorithm x y = euclidAlgorithm y (mod x y) -}


coprime :: Integer -> Integer -> Bool
coprime a b = 
    let
        euclidAlgorithm :: Integer -> Integer -> Integer
        euclidAlgorithm x 0 = x
        euclidAlgorithm x y = euclidAlgorithm y (mod x y)
    in
        if ((a * b) == 0) then error "ZeroException" else if euclidAlgorithm a b == 1 then True else False

