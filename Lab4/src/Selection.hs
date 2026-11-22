module Selection (LineRange(..), Selection, parseSelection, selectLines) where

import Data.List.NonEmpty (NonEmpty)
import LineRange (LineRange(..))

type Selection = NonEmpty LineRange

-- Задание 1. Разберите весь аргумент --lines с помощью Megaparsec.
-- Проверяйте границы через mkRange из LineRange. При ошибке верните сообщение для пользователя.
parseSelection :: String -> Either String Selection
parseSelection = undefined

-- Задание 2. Флаг задаёт выбор дополнения (--invert).
-- Сохраните исходный номер каждой выбранной строки.
selectLines :: Selection -> Bool -> [String] -> [(Integer, String)]
selectLines = undefined
