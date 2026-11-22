{-# LANGUAGE OverloadedStrings #-}
module Properties where

import Hedgehog (Property, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.Hedgehog (testPropertyNamed)

-- Замените каждую заглушку генерацией данных и проверкой свойства,
-- затем снимите её ignoreTestBecause. Добавьте свои свойства.
tests :: TestTree
tests = testGroup "Hedgehog"
    [ testGroup "Streams"
        [ ignoreTestBecause "Реализовать в лабораторной 3" $
            testPropertyNamed "minMax совпадает с minMaxSlow"
                "prop_minMaxReference" prop_minMaxReference
        , ignoreTestBecause "Реализовать в лабораторной 3" $
            testPropertyNamed "fmap id сохраняет префикс потока"
                "prop_streamFunctorIdentity" prop_streamFunctorIdentity
        ]
    , testGroup "FunctorsMonads"
        [ ignoreTestBecause "Реализовать в лабораторной 3" $
            testPropertyNamed "traverseA согласуется с seqA"
                "prop_traverseSequence" prop_traverseSequence
        ]
    ]

-- Для конечных списков целых чисел обе версии дают одинаковый результат.
-- Включите в диапазон длин пустой список.
prop_minMaxReference :: Property
prop_minMaxReference = property $ fail "Реализуйте свойство"

-- Сравните первые n элементов исходного потока и результата fmap id.
prop_streamFunctorIdentity :: Property
prop_streamFunctorIdentity = property $ fail "Реализуйте свойство"

-- Аналогично проверьте закон композиции Functor и все законы Applicative и Monad
-- для Stream: для каждого закона сравните первые n элементов обеих сторон.

-- Для функции f с результатом Maybe сравните traverseA f xs и seqA (map f xs).
-- Подберите данные так, чтобы встречались как успех, так и неудача.
prop_traverseSequence :: Property
prop_traverseSequence = property $ fail "Реализуйте свойство"
