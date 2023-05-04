import FunctorsMonads
import Streams hiding (main)
import Test.Hspec
-- Раскомментируйте QuickCheck или Hegdehog, в зависимости от того, что будете использовать
-- Документация https://hspec.github.io/quickcheck.html
-- import Test.Hspec.QuickCheck
-- Документация в https://github.com/parsonsmatt/hspec-hedgehog#readme
-- import Test.Hspec.Hedgehog

-- Добавьте минимум 5 тестов свойств для функций из первых 2 лабораторных (скопируйте определения тестируемых функций сюда).

main :: IO ()
main = hspec $ do
    describe "functors and monads" $ do
        it "" $ pending
    describe "streams" $ do
        it "" $ pending
