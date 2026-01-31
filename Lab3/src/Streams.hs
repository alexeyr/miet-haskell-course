{-# OPTIONS_GHC -Wall #-}
module Streams where

import Data.List(intercalate)

-- Задание 1 -----------------------------------------

-- Тип Stream a представляет бесконечные списки (потоки) значений типа a
-- (в отличие от [a], которые могут быть как конечными, так и бесконечными
data Stream a = a :> Stream a

-- Экземпляр Show для Stream a печатает первые 10 элементов потока
-- Для использования нужно определить sTake
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ sTake 10 s)
             ++ ", ..."

-- Реализуйте функцию, превращающую поток в (бесконечный) список
streamToList :: Stream a -> [a]
streamToList (x :> xs) = x : streamToList xs

-- функция, возвращающая n первых элементов потока
-- удобна для написания тестов следующих функций
sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (x :> xs) = x : sTake(n-1) xs

-- Задание 2 -----------------------------------------

-- Реализуйте несколько простых функций для работы с потоками
-- Не забудьте добавить тесты!

-- поток, состоящий из одинаковых элементов
sRepeat :: a -> Stream a
sRepeat x = x :> sRepeat x

-- sRepeat 1 == [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...

-- поток, состоящий из бесконечного числа повторов списка
-- (подсказка: эту и предыдущую можно реализовать так, что полученный поток
-- будет циклическим (ссылаться сам на себя), а не бесконечно растущим)
-- sCycle [1, 2, 3] == [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, ...
sCycle :: [a] -> Stream a
sCycle [] = error "sCycle: empty list"
sCycle xs = go xs
  where
    go [] = go xs
    go (y:ys) = y :> go ys

-- поток, заданный начальным значением и функцией, строящей следующее значение
-- по текущему
-- sIterate (/ 2) 1.0 == [1.0, 0.5, 0.25, 0.125, 0.0625, ...
sIterate :: (a -> a) -> a -> Stream a
sIterate f x = x :> sIterate f (f x)

-- функция, возвращающая поток из чередующихся элементов двух потоков
-- (для следующего задания нужно сделать эту функцию ленивой по
-- второму аргументу, то есть не сопоставлять его с образцом)
sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (x :> xs) ys = x :> sInterleave ys xs

-- sInterleave (sRepeat 1) (sRepeat 2) == [1, 2, 1, 2, 1, 2, ...

-- Задание 3 -----------------------------------------

-- Используя предыдущие функции, реализуйте

-- поток натуральных чисел (начиная с 0)
nats :: Stream Integer
nats = go 0
  where go n = n :> go (n+1)

-- nats == [0, 1, 2, 3, 4, 5, 6, 7, ...

-- поток, n-ный элемент которого (начиная с 1) -- максимальная степень 2,
-- делящая n нацело. Подсказка: с помощью sInterleave это можно сделать без
-- проверок на делимость, если её реализация ленива по второму аргументу
-- (подумайте, почему это важно).
ruler :: Stream Integer
ruler = interleaveAll $ map sRepeat [0..]
  where
    interleaveAll (x:xs) = sInterleave x (interleaveAll xs)

-- ruler == [0, 1, 0, 2, 0, 1, 0, 3, ...

-- Задание 4 -----------------------------------------

minMaxSlow, minMax, minMaxBang :: Ord a => [a] -> Maybe (a, a)
{- -O0: Total time: ??? Total Memory in use: ??? -}
{- -O2: Total time: ??? Total Memory in use: ??? -}
minMaxSlow [] = Nothing
minMaxSlow xs = Just (minimum xs, maximum xs)

-- функция minMax должна находить минимальное и максимальное значение списка,
-- так же, как minMaxSlow. Проблема с minMaxSlow в том, что она проходит по списку два раза
-- и поэтому вынуждена сохранять его в памяти целиком. Реализуйте minMax так, чтобы
-- сделать только один проход по списку.

{- -O0: Total time: ??? Total Memory in use: ??? -}
{- -O2: Total time: ??? Total Memory in use: ??? -}
minMax :: Ord a => [a] -> Maybe (a, a)
minMax [] = Nothing
minMax (x:xs) = Just $ foldl (\(mn,mx) y -> (min mn y, max mx y)) (x,x) xs
-- Дополнительное задание: реализуйте ту же самую функцию (под названием minMaxBang) с
-- использованием явной строгости (seq и/или !)

{- -O0: Total time: ??? Total Memory in use: ??? -}
{- -O2: Total time: ??? Total Memory in use: ??? -}
minMaxBang = undefined

-- Скомпилируйте программу с аргументами `ghc Streams.hs -O2 -rtsopts -main-is Streams`
-- и запустите `Streams.exe +RTS -s` (`./Streams +RTS -s` в Linux/OSX).
-- Документацию можете найти в https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html#rts-options-to-produce-runtime-statistics
-- Сравните время выполнения и общую память для разных вариантов.
-- Также посмотрите на эффект при отключении оптимизаций (-O0)

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ ruler
-- main = print $ minMax $ sTake 1000000 $ ruler
-- main = print $ minMaxBang $ sTake 1000000 $ ruler

-- Задание 5 -----------------------------------------

-- Добавьте тесты свойств (на QuickCheck или Hedgehog) для функций из заданий 1--4.
-- http://hackage.haskell.org/package/QuickCheck
-- https://github.com/hedgehogqa/haskell-hedgehog

-- Задание 6 -----------------------------------------

-- Делать после или вместе с FunctorsMonads (задание помещено здесь, чтобы избежать экземпляров-сирот)

-- Реализуйте экземпляры классов для потоков (модуль Streams)
-- Проверьте законы! Здесь легко написать реализации, которая подходит по типам, но законы для неё не выполняются.
-- Подсказка: тип Stream a изоморфен Nat -> a (Nat -- тип натуральных чисел).
-- Для тестирования законов можно использовать библиотеку quickcheck-classes (не входит в Haskell Platform):
-- http://hackage.haskell.org/package/quickcheck-classes
-- или http://hackage.haskell.org/package/hedgehog-classes, если в предыдущем задании использовали Hedgehog.

 instance Functor Stream where
    fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
    pure x = xs where xs = x :> xs
    (f :> fs) <*> (x :> xs) = f x :> (fs <*> xs)


instance Monad Stream where
    return = pure
    xs >>= f = joinStream (fmap f xs)

joinStream :: Stream (Stream a) -> Stream a
joinStream (x :> xs) = interleaveStreams x (joinStream xs)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (a :> as) bs = a :> interleaveStreams bs as


instance Foldable Stream where
    foldr f z (x :> xs) = f x (foldr f z xs)
    
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Traversable.html
instance Traversable Stream where
    traverse f (x :> xs) = liftA2 ( :> ) (f x) (traverse f xs)    
    -- достаточно определить одну из них
    -- traverse = undefined
    -- sequenceA = undefined
