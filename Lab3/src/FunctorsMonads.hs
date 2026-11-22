module FunctorsMonads where

-- определяем аналоги стандартных классов Functor и Monad,
-- чтобы нам не мешали их существующие экземпляры

-- документация на стандартные версии:
-- https://hackage.haskell.org/package/base/docs/Data-Functor.html
-- https://hackage.haskell.org/package/base/docs/Control-Applicative.html
-- https://hackage.haskell.org/package/base/docs/Control-Monad.html
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

-- Реализуйте оба метода по умолчанию: join' через >>== и наоборот.
-- В каждом экземпляре определите хотя бы один из них, иначе методы зациклятся.
class Applicative' m => Monad' m where
  {-# MINIMAL (>>==) | join' #-}
  (>>==) :: m a -> (a -> m b) -> m b
  (>>==) = error "implement using join' (and Applicative')"
  join' :: m (m a) -> m a
  join' = error "implement using >>== (and Applicative')"

infixl 1 >>==

-- Примеры: Maybe получает join' по умолчанию, а [] получает >>==.
instance Functor' Maybe where
  _ <$$> Nothing = Nothing
  f <$$> Just x = Just (f x)
instance Applicative' Maybe where
  pure' = Just
  Just f <**> Just x = Just (f x)
  _ <**> _ = Nothing
instance Monad' Maybe where
  Nothing >>== _ = Nothing
  Just x >>== f = f x

instance Functor' [] where
  f <$$> xs = map f xs
instance Applicative' [] where
  pure' x = [x]
  fs <**> xs = [f x | f <- fs, x <- xs]
instance Monad' [] where
  join' xss = [x | xs <- xss, x <- xs]

-- Задание 2 -----------------------------------------

-- Реализуйте функции, следуя типам.
-- Попробуйте восстановить liftA2' из лекции и использовать в следующих функциях.

-- Добавьте тесты! Используйте не только те примеры, которые приведены здесь.
-- Попробуйте написать тесты _до_ реализации, чтобы проверить, правильных ли
-- результатов вы ожидаете. Свойства добавляйте в test/Properties.hs.

-- "Поднимает" функцию от двух аргументов в функцию, работающую с действиями
-- liftA2' (+) (Just 1) (Just 2) == Just 3
-- liftA2' (+) Nothing (Just 2) == Nothing
liftA2' :: Applicative' f => (a -> b -> c) -> f a -> f b -> f c
liftA2' = undefined

-- Применяет функцию, возвращающую действия, ко всем элементам списка, выполняет эти действия
-- и собирает результаты в список
-- traverseA Just [1, 2] == Just [1, 2]
-- traverseA (\a -> if a > 2 then Just a else Nothing) [1, 3] == Nothing
traverseA :: Applicative' f => (a -> f b) -> [a] -> f [b]
traverseA = undefined

-- Фильтрует список, используя "предикат с эффектом".
-- filterA (\a -> if a > 10 then Nothing else Just (a > 0)) [-1, -2, 1, 2] == Just [1, 2]
-- filterA (\a -> if a < 0 then Nothing else Just (a > 1)) [-1, -2, 1, 2] == Nothing
filterA :: Applicative' f => (a -> f Bool) -> [a] -> f [a]
filterA = undefined

-- Найдите первый элемент, для которого предикат с эффектом возвращает True.
-- Проверяйте элементы слева направо. После совпадения остальные не проверяйте.
-- Если совпадений нет, результат поиска внутри действия — Nothing.
-- В GHCi: import Text.Read (readMaybe)
-- let positive s = (> 0) <$> readMaybe s
-- findM positive ["-2", "3", "bad"] == Just (Just "3")
-- findM positive ["-2", "bad", "3"] == Nothing
-- findM positive ["-2", "0"] == Just Nothing
findM :: Monad' m => (a -> m Bool) -> [a] -> m (Maybe a)
findM = undefined

-- Вопрос: почему для произвольного Applicative' нельзя определить findA
-- с тем же поведением, включая остановку проверок после первого совпадения?

-- Задание 3 -----------------------------------------

-- Реализуйте экземпляры классов типов

-- Добавьте тесты на поведение функций из задания 2 с этими экземплярами

-- Значения хранятся в листьях. Отображение сохраняет форму дерева.
-- При связывании каждый лист заменяется деревом, которое возвращает функция.
-- Согласуйте <**> с таким связыванием.
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor' Tree where
  (<$$>) = undefined
instance Applicative' Tree where
  pure' = undefined
  (<**>) = undefined
instance Monad' Tree where

instance Functor' ((->) t) where -- (->) a b -- то же самое, что a -> b
  (<$$>) = undefined
instance Applicative' ((->) t) where
  pure' = undefined
  (<**>) = undefined
instance Monad' ((->) t) where


-- Задание 4 -----------------------------------------

-- В test/ClassLaws.hs изучите functorLaws и applicativeLaws. Запустите их
-- для готовых экземпляров Maybe и []. После задания 3 включите также группы
-- для Tree и функций, сняв их ignoreTestBecause.
-- По этому образцу реализуйте monadLaws: проверьте левую и правую единицы,
-- ассоциативность >>== и согласованность >>== с <**>. Затем снимите
-- ignoreTestBecause перед monadLaws и запустите проверки для всех экземпляров.

-- Необязательное задание: независимые проверки -------------------------

data Validation a = Failure [String] | Success a
  deriving (Eq, Show)

-- Реализуйте Functor' и Applicative' для Validation. Если все проверки
-- успешны, объедините их результаты. При ошибках соберите все сообщения
-- в порядке проверок слева направо.
-- Например, liftA2' (,) объединяет результаты проверок имени и возраста:
-- если имя пустое и возраст отрицательный, нужны оба сообщения об ошибках.
-- Добавьте примеры и включите группу Validation в test/ClassLaws.hs.
-- Можно ли определить >>== так, чтобы он согласовывался с таким <**>?
instance Functor' Validation where
  (<$$>) = undefined
instance Applicative' Validation where
  pure' = undefined
  (<**>) = undefined
