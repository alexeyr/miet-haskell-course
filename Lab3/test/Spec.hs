import qualified Properties
import FunctorsMonads (Applicative'(..), Monad'(..), Tree(..), Validation(..), findM, liftA2')
import Text.Read (readMaybe)
import Test.Hspec
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Hspec (TreatPendingAs(..), testSpec)

main :: IO ()
main = do
    examples <- testSpec "Hspec" spec
    defaultMain $ testGroup "Lab3"
        [ localOption TreatPendingAsSuccess examples
        , Properties.tests
        ]

spec :: Spec
spec = do
    describe "Monad'" $ do
        it "join' для Maybe через метод по умолчанию" $ do
            pending
            join' (Just (Just True)) `shouldBe` Just True
        it ">>== для списков через метод по умолчанию" $ do
            pending
            ([1, 2] >>== replicate 2) `shouldBe` [1, 1, 2, 2 :: Int]
    describe "findM" $ do
        let positive s = (> 0) <$> (readMaybe s :: Maybe Integer)
        it "останавливается на первом совпадении" $ do
            pending
            findM positive ["-2", "3", "bad"] `shouldBe` Just (Just "3")
        it "сохраняет ошибку проверки до первого совпадения" $ do
            pending
            findM positive ["-2", "bad", "3"] `shouldBe` Nothing
        it "возвращает Just Nothing, если все проверки дали False" $ do
            pending
            findM positive ["-2", "0"] `shouldBe` Just Nothing
    describe "Экземпляры задания 3" $ do
        it "связывание заменяет каждый лист полученным деревом" $ do
            pending
            let branch x = if x then Node (Leaf 1) (Leaf 2) else Leaf (0 :: Int)
            (Node (Leaf True) (Leaf False) >>== branch)
                `shouldBe` Node (Node (Leaf 1) (Leaf 2)) (Leaf 0)
        it "Обе функции получают одно и то же окружение" $ do
            pending
            (((+) <**> (* 2)) 3) `shouldBe` (9 :: Int)
    describe "Validation (необязательно)" $ do
        it "собирает ошибки независимых проверок слева направо" $ do
            pending
            let name = Failure ["пустое имя"] :: Validation String
                age = Failure ["отрицательный возраст"] :: Validation Int
            liftA2' (,) name age `shouldBe` Failure ["пустое имя", "отрицательный возраст"]
    describe "Streams" $ do
        it "sTake возвращает пустой список при n <= 0" pending
        it "sInterleave чередует элементы двух потоков" pending
        describe "Экземпляры задания 6" $ do
            it "fmap преобразует каждый элемент префикса" pending
            it "<*> применяет функции к элементам на тех же позициях" pending
            it ">>= выбирает элемент результирующего потока на той же позиции" pending
            it "foldr может вернуть результат по первому элементу" pending
            it "необязательно: traverse с Maybe замечает Nothing на конечной позиции" pending
