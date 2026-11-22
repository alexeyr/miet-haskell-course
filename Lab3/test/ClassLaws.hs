module ClassLaws (tests, functorLaws, applicativeLaws, monadLaws) where

import Data.String (fromString)
import FunctorsMonads
import Hedgehog (Gen, PropertyT, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.Hedgehog (testPropertyNamed)

-- draw получает генератор элементов и выбирает значение в контексте f.
-- same сравнивает два результата. Для функций сравним их в каждой точке Bool.
type Draw f = forall a. Show a => Gen a -> PropertyT IO (f a)
type Same f = f Bool -> f Bool -> PropertyT IO ()

law :: String -> PropertyT IO () -> TestTree
law name = testPropertyNamed name (fromString name) . property

-- У функции Bool -> a всего два результата. Таблицу удобно выводить
-- в контрпримере, а из неё можно восстановить саму функцию.
at :: (a, a) -> Bool -> a
at (onFalse, onTrue) x = if x then onTrue else onFalse

table :: Gen a -> Gen (a, a)
table gen = (,) <$> gen <*> gen

functorLaws :: Functor' f => Draw f -> Same f -> TestTree
functorLaws draw same = testGroup "Functor'"
    [ law "identity" $ do
        xs <- draw Gen.bool
        same (id <$$> xs) xs
    , law "composition" $ do
        xs <- draw Gen.bool
        f <- at <$> forAll (table Gen.bool)
        g <- at <$> forAll (table Gen.bool)
        same ((f . g) <$$> xs) (f <$$> (g <$$> xs))
    ]

applicativeLaws :: Applicative' f => Draw f -> Same f -> TestTree
applicativeLaws draw same = testGroup "Applicative'"
    [ law "identity" $ do
        v <- draw Gen.bool
        same (pure' id <**> v) v
    , law "homomorphism" $ do
        f <- at <$> forAll (table Gen.bool)
        x <- forAll Gen.bool
        same (pure' f <**> pure' x) (pure' (f x))
    , law "interchange" $ do
        u <- drawFunctions
        y <- forAll Gen.bool
        same (u <**> pure' y) (pure' ($ y) <**> u)
    , law "composition" $ do
        u <- drawFunctions
        v <- drawFunctions
        w <- draw Gen.bool
        same (pure' (.) <**> u <**> v <**> w) (u <**> (v <**> w))
    , law "functor compatibility" $ do
        f <- at <$> forAll (table Gen.bool)
        v <- draw Gen.bool
        same (pure' f <**> v) (f <$$> v)
    ]
  where
    drawFunctions = do
        tables <- draw (table Gen.bool)
        pure (at <$$> tables)

-- Задание 4 из FunctorsMonads.hs. Оформите каждый закон отдельным свойством.
-- Чтобы получить функцию Bool -> m Bool, выберите два значения через
-- draw Gen.bool и используйте их как таблицу результатов.
monadLaws :: Monad' m => Draw m -> Same m -> TestTree
monadLaws _draw _same = testGroup "Monad'"
    [ law "left identity" $ fail "Реализуйте проверку левой единицы"
    , law "right identity" $ fail "Реализуйте проверку правой единицы"
    , law "associativity" $ fail "Реализуйте проверку ассоциативности"
    , law "applicative compatibility" $ fail "Проверьте согласованность с <**>"
    ]

tests :: TestTree
tests = testGroup "Законы классов"
    [ forInstance "Maybe" (forAll . Gen.maybe) (===)
    , forInstance "[]" (forAll . Gen.list (Range.linear 0 4)) (===)
    , ignoreTestBecause "Реализуйте экземпляры Tree из задания 3" $
        forInstance "Tree" (forAll . genTree) (===)
    , ignoreTestBecause "Реализуйте экземпляры функций из задания 3" $
        forInstance "Bool ->" drawReader sameReader
    , ignoreTestBecause "Необязательно: реализуйте экземпляры Validation" $
        testGroup "Validation"
            [ functorLaws drawValidation (===)
            , applicativeLaws drawValidation (===)
            ]
    ]
  where
    drawValidation :: Draw Validation
    drawValidation gen = forAll $ Gen.choice
        [ Success <$> gen
        , Failure <$> Gen.list (Range.linear 0 3) (Gen.element ["имя", "возраст"])
        ]

    genTree :: Gen a -> Gen (Tree a)
    genTree gen = tree (3 :: Int)
      where
        leaf = Leaf <$> gen
        tree 0 = leaf
        tree depth = Gen.choice [leaf, Node <$> child <*> child]
          where child = tree (depth - 1)

    drawReader :: Draw ((->) Bool)
    drawReader gen = at <$> forAll (table gen)

    sameReader :: Same ((->) Bool)
    sameReader f g = (f False, f True) === (g False, g True)

forInstance :: Monad' f => String -> Draw f -> Same f -> TestTree
forInstance name draw same = testGroup name
    [ functorLaws draw same
    , applicativeLaws draw same
    , ignoreTestBecause "Реализуйте monadLaws в задании 4" $
        monadLaws draw same
    ]
