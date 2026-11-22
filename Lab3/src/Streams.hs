{-# OPTIONS_GHC -Wall #-}
module Streams where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))

-- Задание 1 -----------------------------------------

-- Тип Stream a представляет бесконечные списки (потоки) значений типа a
-- (в отличие от [a], которые могут быть как конечными, так и бесконечными).
data Stream a = a :> Stream a

-- Экземпляр Show для Stream a печатает первые 10 элементов потока
-- Для использования нужно определить sTake
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ sTake 10 s)
             ++ ", ..."

-- Реализуйте функцию, превращающую поток в (бесконечный) список
streamToList :: Stream a -> [a]
streamToList = undefined

-- функция, возвращающая n первых элементов потока
-- при n <= 0 верните []. Используйте sTake в тестах следующих функций.
sTake :: Int -> Stream a -> [a]
sTake = undefined

-- Задание 2 -----------------------------------------

-- Реализуйте несколько простых функций для работы с потоками
-- Не забудьте добавить тесты!

-- поток, состоящий из одинаковых элементов
sRepeat :: a -> Stream a
sRepeat = undefined

-- sTake 4 (sRepeat 1) == [1, 1, 1, 1]

-- поток, состоящий из бесконечного числа повторов непустого списка
-- (подсказка: эту и предыдущую можно реализовать так, что полученный поток
-- будет циклическим (ссылаться сам на себя), а не бесконечно растущим)
-- sTake 5 (sCycle (1 :| [2, 3])) == [1, 2, 3, 1, 2]
sCycle :: NonEmpty a -> Stream a
sCycle = undefined

-- поток, заданный начальным значением и функцией, строящей следующее значение
-- по текущему
-- sTake 4 (sIterate (/ 2) 1.0) == [1.0, 0.5, 0.25, 0.125]
sIterate :: (a -> a) -> a -> Stream a
sIterate = undefined

-- функция, возвращающая поток из чередующихся элементов двух потоков
-- (для следующего задания нужно сделать эту функцию ленивой по
-- второму аргументу, то есть не сопоставлять его с образцом)
sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (_ :> _) _ = undefined

-- sTake 5 (sInterleave (sRepeat 1) (sRepeat 2)) == [1, 2, 1, 2, 1]

-- Задание 3 -----------------------------------------

-- Используя предыдущие функции, реализуйте

-- поток натуральных чисел (начиная с 0)
nats :: Stream Integer
nats = undefined

-- sTake 5 nats == [0, 1, 2, 3, 4]

-- Элемент на позиции n (начиная с 1) — показатель наибольшей степени двойки,
-- которая делит n нацело. Подсказка: с помощью sInterleave это можно сделать без
-- проверок на делимость, если её реализация ленива по второму аргументу
-- (подумайте, почему это важно).
ruler :: Stream Integer
ruler = undefined

-- sTake 8 ruler == [0, 1, 0, 2, 0, 1, 0, 3]

-- Задание 4 -----------------------------------------

minMaxSlow, minMax, minMaxBang :: Ord a => [a] -> Maybe (a, a)
{- -O0: Total time: ???  bytes allocated in the heap: ???  maximum residency: ??? -}
{- -O2: Total time: ???  bytes allocated in the heap: ???  maximum residency: ??? -}
minMaxSlow [] = Nothing
minMaxSlow xs = Just (minimum xs, maximum xs)

-- функция minMax должна находить минимальное и максимальное значение списка,
-- так же, как minMaxSlow. Проблема с minMaxSlow в том, что она проходит по списку два раза
-- и поэтому вынуждена сохранять его в памяти целиком. Реализуйте minMax так, чтобы
-- сделать только один проход по списку.

{- -O0: Total time: ???  bytes allocated in the heap: ???  maximum residency: ??? -}
{- -O2: Total time: ???  bytes allocated in the heap: ???  maximum residency: ??? -}
minMax = undefined

-- Дополнительное задание: реализуйте ту же самую функцию (под названием minMaxBang) с
-- использованием явной строгости (seq и/или !)

{- -O0: Total time: ???  bytes allocated in the heap: ???  maximum residency: ??? -}
{- -O2: Total time: ???  bytes allocated in the heap: ???  maximum residency: ??? -}
minMaxBang = undefined

-- В каталоге Lab3 выберите в main одну реализацию. Соберите её командой
-- stack ghc -- src/Streams.hs -O0 -rtsopts -main-is Streams -outputdir .stack-work -o .stack-work/minmax.exe
-- и запустите ./.stack-work/minmax.exe +RTS -s. Повторите с -O2.
-- Для каждого варианта заново соберите и запустите программу на том же входе.
-- Для minMaxBang измерения нужны только при выполнении необязательного задания.
-- Запишите Total time (первое число), bytes allocated in the heap и maximum residency.
-- Второй показатель суммирует все выделения памяти, третий оценивает максимум
-- живых данных. По какому из них видно, удерживает ли функция список в памяти?

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ ruler
-- main = print $ minMax $ sTake 1000000 $ ruler
-- main = print $ minMaxBang $ sTake 1000000 $ ruler

-- Задание 5 -----------------------------------------

-- В test/Properties.hs напишите свойства Hedgehog для функций из заданий 1–4.
-- В качестве эталонов можно использовать функции над списками и minMaxSlow.

-- Задание 6 -----------------------------------------

-- Выполняйте после FunctorsMonads или параллельно с ним.
-- Здесь реализуются стандартные классы для Stream. Подсказка: Stream a
-- изоморфен Nat -> a (Nat — тип натуральных чисел).
-- Проверьте законы на конечных префиксах: генерируйте небольшую длину n
-- и сравнивайте sTake n для обеих сторон равенства. Потоки можно строить,
-- например, повторением сгенерированного непустого списка. Для законов Monad
-- используйте также потоки, в которых значения меняются от позиции к позиции.
-- Для Foldable выбирайте наблюдения, которым достаточно конечного префикса.

instance Functor Stream where
    fmap = undefined

instance Applicative Stream where
    pure = undefined
    (<*>) = undefined

instance Monad Stream where
    -- в этом случае может быть проще использовать реализацию через join
    -- xs >>= f = join ... where join = ...
    (>>=) = undefined

-- https://hackage.haskell.org/package/base/docs/Data-Foldable.html
instance Foldable Stream where
    -- достаточно определить одну из них
    -- foldr = undefined
    -- foldMap = undefined

-- Необязательно: реализуйте Traversable Stream. Что произойдёт при
-- traverse print nats? Будет ли виден какой-либо результат до завершения обхода?
-- https://hackage.haskell.org/package/base/docs/Data-Traversable.html
-- instance Traversable Stream where
--     traverse = undefined
