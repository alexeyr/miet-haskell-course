module CLI where

import Options.Applicative (Parser, ParserInfo, fullDesc, helper, info, progDesc)
import Selection (Selection)

data Options = Options
  { selection :: Selection
  , numbered :: Bool
  , inverted :: Bool
  , inputFile :: Maybe FilePath
  } deriving (Eq, Show)

-- Задание 3. Опишите аргументы pick-lines средствами optparse-applicative.
options :: Parser Options
options = undefined

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> options)
  (fullDesc <> progDesc "Вывести выбранные строки текста")

-- Задание 4. Прочитайте текст из указанного источника и выведите результат.
run :: Options -> IO ()
run = undefined
