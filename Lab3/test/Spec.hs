import qualified Properties
import FunctorsMonads (Monad'(..))
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
    describe "Streams" $ do
        it "sTake возвращает пустой список при n <= 0" pending
        it "sInterleave чередует элементы двух потоков" pending
