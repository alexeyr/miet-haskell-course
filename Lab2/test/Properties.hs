{-# LANGUAGE OverloadedStrings #-}
module Properties where

import Hedgehog (Property, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.Hedgehog (testPropertyNamed)

-- Эти свойства предстоит реализовать в лабораторной 3.
-- Для каждого свойства замените заглушку генерацией входных данных
-- и проверкой, затем снимите соответствующий ignoreTestBecause ниже.
-- Список не исчерпывающий: добавьте собственные свойства.
-- Каждая заглушка соответствует одному свойству, а не всей операции.
-- Для одной операции могут понадобиться несколько свойств;
-- придумывайте их и для операций, не представленных в этой заготовке.
-- Заглушки намеренно завершаются ошибкой, если включить их без реализации.

tests :: TestTree
tests = testGroup "Hedgehog (лабораторная 3)"
    [ ignoreTestBecause "Реализовать в лабораторной 3" $
        testPropertyNamed "Прибавление константы сдвигает значение многочлена"
            "prop_addConstant" prop_addConstant
    , testGroup "RootFinding"
        [ ignoreTestBecause "Реализовать в лабораторной 3" $
            testPropertyNamed "Шаг Ньютона находит корень линейной функции"
                "prop_newtonStepLinear" prop_newtonStepLinear
        , ignoreTestBecause "Реализовать в лабораторной 3" $
            testPropertyNamed "Результат Ньютона удовлетворяет заданной невязке"
                "prop_newtonResidual" prop_newtonResidual
        , ignoreTestBecause "Необязательно: реализовать в лабораторной 3" $
            testPropertyNamed "Результат Хэлли удовлетворяет заданной невязке"
                "prop_halleyResidual" prop_halleyResidual
        ]
    , testGroup "SimpleLang"
        [ ignoreTestBecause "Реализовать в лабораторной 3" $
            testPropertyNamed "Чтение после обновления переменной"
                "prop_extendLookup" prop_extendLookup
        , ignoreTestBecause "Реализовать в лабораторной 3" $
            testPropertyNamed "Обновление сохраняет остальные переменные"
                "prop_extendOther" prop_extendOther
        , ignoreTestBecause "Реализовать в лабораторной 3" $
            testPropertyNamed "Вычисление суммы выражений"
                "prop_evalPlus" prop_evalPlus
        , ignoreTestBecause "Реализовать в лабораторной 3" $
            testPropertyNamed "Incr увеличивает переменную на единицу"
                "prop_runIncr" prop_runIncr
        , ignoreTestBecause "Реализовать в лабораторной 3" $
            testPropertyNamed "Объединение блоков сохраняет порядок выполнения"
                "prop_blockSequence" prop_blockSequence
        , ignoreTestBecause "Реализовать в лабораторной 3" $
            testPropertyNamed "factorial совпадает с реализацией на Haskell"
                "prop_factorialReference" prop_factorialReference
        , ignoreTestBecause "Реализовать в лабораторной 3" $
            testPropertyNamed "Результат squareRoot ограничивает вход соседними квадратами"
                "prop_squareRootBounds" prop_squareRootBounds
        ]
    ]

-- Прибавление к многочлену p константы c увеличивает его значение
-- в любой точке t на c. Используйте коэффициенты и точки типа Integer.
prop_addConstant :: Property
prop_addConstant = property $ fail "Реализуйте свойство"

-- Для линейной функции с ненулевым коэффициентом при переменной один шаг
-- Ньютона из любой точки даёт её корень. Используйте Rational.
prop_newtonStepLinear :: Property
prop_newtonStepLinear = property $ fail "Реализуйте свойство"

-- Если newton возвращает Just t, невязка в точке t не превышает epsilon.
-- Выбирайте допустимые аргументы и проверяйте, что генераторы дают также
-- случаи успешного поиска: одни результаты Nothing не проверяют это свойство.
prop_newtonResidual :: Property
prop_newtonResidual = property $ fail "Реализуйте свойство"

-- Необязательно: аналогичное свойство невязки для метода Хэлли.
prop_halleyResidual :: Property
prop_halleyResidual = property $ fail "Реализуйте свойство"

-- Для следующих свойств состояние можно генерировать как список обновлений empty
-- или как функцию String -> Integer с помощью hedgehog-fn:
-- https://hackage.haskell.org/package/hedgehog-fn/docs/Hedgehog-Function.html

-- После extend чтение обновлённой переменной возвращает записанное значение.
prop_extendLookup :: Property
prop_extendLookup = property $ fail "Реализуйте свойство"

-- Обновление одной переменной не меняет значение другой.
prop_extendOther :: Property
prop_extendOther = property $ fail "Реализуйте свойство"

-- Значение суммы двух выражений равно сумме их значений в том же состоянии.
-- Генерируйте выражения без деления на ноль.
prop_evalPlus :: Property
prop_evalPlus = property $ fail "Реализуйте свойство"

-- Выполнение Incr name через run увеличивает значение
-- name на единицу и сохраняет значение любой другой переменной.
prop_runIncr :: Property
prop_runIncr = property $ fail "Реализуйте свойство"

-- Выполнение блока из объединения двух списков инструкций даёт тот же
-- результат, что последовательное выполнение двух отдельных блоков.
-- Используйте программы без циклов и деления на ноль.
prop_blockSequence :: Property
prop_blockSequence = property $ fail "Реализуйте свойство"

-- Для неотрицательного входа In программа factorial должна записывать в Out
-- тот же результат, что и функция вычисления факториала, написанная на Haskell.
-- Ограничьте диапазон входов, чтобы тесты выполнялись быстро.
prop_factorialReference :: Property
prop_factorialReference = property $ fail "Реализуйте свойство"

-- Для неотрицательного входа A результат B программы squareRoot
-- удовлетворяет B >= 0 и B^2 <= A < (B + 1)^2.
-- Ограничьте диапазон A, чтобы тесты не тратили слишком много времени на цикл.
prop_squareRootBounds :: Property
prop_squareRootBounds = property $ fail "Реализуйте свойство"
