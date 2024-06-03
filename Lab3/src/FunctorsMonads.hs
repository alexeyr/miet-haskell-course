module FunctorsMonads where

-- определяем аналоги стандартных классов Functor и Monad,
-- чтобы нам не мешали их существующие экземпляры

-- документация на стандартные версии:
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor.html
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html
-- заметьте, что посмотрев в источники, можно взять реализацию оттуда;
-- постарайтесь этого не делать.

class Functor' f where
  (<$$>) :: (a -> b) -> f a -> f b

infixl 4 <$$>

class Functor' f => Applicative' f where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b

infixl 4 <**>

-- Задание 1 -----------------------------------------

-- реализуйте join' через >>== и наоборот
class Applicative' m => Monad' m where
  (>>==) :: Applicative m => m a -> (a -> m b) -> m b
  x >>== m = join' (fmap m x)
  join' :: Applicative m => m (m a) -> m a
  join' x = x >>== id

-- пример
instance Functor' Maybe where
  _ <$$> Nothing = Nothing
  f <$$> Just x = Just (f x)
instance Applicative' Maybe where
  pure' = Just
  Just f <**> Just x = Just (f x)
  _ <**> _ = Nothing
instance Monad' Maybe where
  -- достаточно было бы определить одну из функций >>== и join'
  Nothing >>== _ = Nothing
  Just x >>== f = f x
  join' Nothing = Nothing
  join' (Just x) = x

instance Functor' [] where
  f <$$> xs = map f xs
instance Applicative' [] where
  pure' x = [x]
  fs <**> xs = [f x | f <- fs, x <- xs]
instance Monad' [] where
  -- достаточно было бы определить одну из функций >>== и join'
  xs >>== f = [y | x <- xs, y <- f x]
  join' xss = [x | xs <- xss, x <- xs]

-- Задание 2 -----------------------------------------

-- Реализуйте функции, следуя типам.
-- liftA2' и seqA были на лекции, но:
-- 1. можно попробовать восстановить самостоятельно
-- 2. они могут быть использованы для других заданий

-- Добавьте тесты! Используйте не только те примеры, которые приведены здесь.
-- Попробуйте написать тесты _до_ реализации, чтобы проверить, правильных ли
-- результатов вы ожидаете. Можно также использовать тесты свойств.

-- "Поднимает" функцию от двух аргументов в функцию, работающую с действиями
-- liftA2' (+) (Just 1) (Just 2) == Just 3
-- liftA2' (+) Nothing (Just 2) == Nothing
liftA2' :: Applicative' f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f fa fb = f <$$> fa <**> fb

-- Выполняет все действия в списке и собирает их результаты в один список
-- seqA [Just 1, Just 2] == Just [1, 2]
-- seqA [Just 1, Just 2, Nothing] == Nothing
seqA :: Applicative' f => [f a] -> f [a]
seqA [] = pure' []
seqA (x:xs) = (:) <$$> x <**> seqA xs

-- Применяет функцию, возвращающую действия, ко всем элементам списка, выполняет эти действия
-- и собирает результаты в список
-- traverseA Just [1, 2] == Just [1, 2]
-- traverseA (\a -> if a > 2 then Just a else Nothing) [1, 3] == Nothing
traverseA :: Applicative' f => (a -> f b) -> [a] -> f [b]
traverseA f = seqA . fmap f

-- Фильтрует список, используя "предикат с эффектом".
-- filterA (\a -> if a > 10 then Nothing else Just (a > 0)) [-1, -2, 1, 2] == Just [1, 2]
-- filterA (\a -> if a < 0 then Nothing else Just (a > 1)) [-1, -2, 1, 2] == Nothing
filterA :: Applicative' f => (a -> f Bool) -> [a] -> f [a]
filterA predicate = foldr (\x -> liftA2' (\cond -> if cond then (x:) else id) (predicate x)) (pure' [])

-- Композиция монадических функций
-- composeM Just Just == Just (т.е. для всех x: composeM Just Just x == Just x)
-- composeM Just (const Nothing) == const Nothing
-- composeM :: Applicative' m => (b -> m c) -> (a -> m b) -> (a -> m c)
-- composeM f g = \x -> g x >>== f 

-- Задание 3 -----------------------------------------

-- Реализуйте экземпляры классов типов

-- Добавьте тесты на поведение функций из задания 2 с этими экземплярами

instance Functor' (Either t) where
  _ <$$> Left l = Left l
  f <$$> Right r = Right (f r) 

instance Applicative' (Either t) where
  pure' = Right
  Left l <**> _ = Left l
  Right f <**> r = fmap f r

instance Monad' (Either t) where
  (Left l) >>== _ = Left l
  (Right r) >>== f = f r

instance Functor' ((->) t) where -- (->) a b -- то же самое, что a -> b
  f <$$> g = \x -> f (g x)

instance Applicative' ((->) t) where
  pure' = const
  (<**>) f g x = f x (g x)

instance Monad' ((->) t) where
  f >>== k = \r -> k (f r) r
