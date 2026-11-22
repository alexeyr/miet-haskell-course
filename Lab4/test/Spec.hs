module Main where

import CLI (Options(..), parserInfo)
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty(..))
import LineRange (mkRange)
import Options.Applicative (ParserResult(..), defaultPrefs, execParserPure)
import Selection
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "mkRange" $ do
    it "возвращает исходные границы, в том числе совпадающие" $ do
      pending
      mkRange 2 5 `shouldBe` Just (LineRange 2 5)
      mkRange 1 1 `shouldBe` Just (LineRange 1 1)
    it "отклоняет неположительную левую границу и обратный диапазон" $ do
      pending
      mkRange 0 2 `shouldBe` Nothing
      mkRange 3 2 `shouldBe` Nothing

  describe "parseSelection" $ do
    it "разбирает номера и включительные диапазоны" $ do
      pending
      parseSelection "1,3-5,8" `shouldBe`
        Right (LineRange 1 1 :| [LineRange 3 5, LineRange 8 8])
    it "отклоняет пустой выбор, ноль, обратный диапазон и лишние символы" $ do
      pending
      traverse_ (\s -> parseSelection s `shouldSatisfy` isLeft)
        ["", "0", "5-3", "1,,3", "2-4x"]

  describe "selectLines" $ do
    it "сохраняет порядок строк и выбирает каждую строку один раз" $ do
      pending
      selectLines (LineRange 3 5 :| [LineRange 2 3]) False ["a", "", "c"]
        `shouldBe` [(2, ""), (3, "c")]
    it "при --invert сохраняет исходные номера" pending
    it "обрабатывает пустой ввод" pending
    it "Hedgehog: диапазон даёт те же строки, что и перечисление его номеров" pending

  describe "Аргументы командной строки" $ do
    it "принимает только обязательный аргумент и использует стандартный ввод" $ do
      pending
      case execParserPure defaultPrefs parserInfo ["--lines", "2"] of
        Success opts -> opts `shouldBe` Options (LineRange 2 2 :| []) False False Nothing
        _ -> expectationFailure "Ожидался успешный разбор"
    it "принимает имя файла и оба флага вместе" pending
    it "сообщает об ошибке при отсутствии --lines или неверном выборе" pending

  describe "Ввод-вывод" $ do
    it "читает стандартный ввод и выводит только текст без --number" $ do
      pending
      -- Последний аргумент readProcessWithExitCode передаётся программе на stdin.
      (status, output, errors) <- readProcessWithExitCode
        "pick-lines" ["--lines", "1,3-4"] (unlines ["milk", "eggs", "bread", "tea"])
      (status, errors) `shouldBe` (ExitSuccess, "")
      output `shouldBe` unlines ["milk", "bread", "tea"]
    it "читает файл и выводит выбранные строки с номерами" pending
