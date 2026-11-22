module Selection where

import Data.List.NonEmpty (NonEmpty)

-- Обе границы включены. Один номер n представляется как LineRange n n.
data LineRange = LineRange Integer Integer
  deriving (Eq, Show)

type Selection = NonEmpty LineRange

-- Задание 1. Разберите весь аргумент --lines с помощью Megaparsec.
-- При ошибке верните сообщение для пользователя.
parseSelection :: String -> Either String Selection
parseSelection = undefined

-- Задание 2. Флаг задаёт выбор дополнения (--invert).
-- Сохраните исходный номер каждой выбранной строки.
selectLines :: Selection -> Bool -> [String] -> [(Integer, String)]
selectLines = undefined
