module SimpleLang where
-- Язык Simple -- очень простой императивный язык.
-- В нём только один тип данных: целые числа Integer без переполнения.

data Expression =
    Var String                   -- Переменные
  | Val Integer                  -- Целые константы
  | Op Expression Bop Expression -- Бинарные операции
  deriving (Show, Eq)

data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt       -- >
  | Ge       -- >=
  | Lt       -- <
  | Le       -- <=
  | Eql      -- ==
  deriving (Show, Eq)

data Statement =
    -- присвоить переменной значение выражения
    Assign   String     Expression
    -- увеличить переменную на единицу
  | Incr     String
    -- ненулевые значения работают как истина в if, while и for
  | If       Expression Statement  Statement
  | While    Expression Statement
    -- For initialization condition step body:
    -- initialization выполняется один раз, затем проверяется condition;
    -- пока оно ненулевое, выполняются body и step (именно в таком порядке).
  | For      Statement  Expression Statement Statement
    -- как { ... } в C-подобных языках; Block [] ничего не делает
  | Block [Statement]
    -- пустая инструкция
  | Skip
  deriving (Show, Eq)

-- примеры программ на этом языке в конце модуля

-- по состоянию можно получить значение каждой переменной
-- (в реальной программе скорее использовалось бы Data.Map.Map String Integer)
type State = String -> Integer

-- Задание 1 -----------------------------------------

-- в начальном состоянии все переменные имеют значение 0
empty :: State
empty = undefined

-- возвращает состояние, в котором переменная var имеет значение newVal, 
-- все остальные -- то же, что в state
extend :: State -> String -> Integer -> State
extend state var newVal = undefined

-- Задание 2 -----------------------------------------

-- возвращает значение выражения expr при значениях переменных из state.
-- Сравнения возвращают 1, если условие истинно, и 0 иначе.
-- Divide использует div (округление вниз): (-3) `div` 2 == -2.
-- Деление на ноль вызывает стандартную ошибку div; обрабатывать её не требуется.
eval :: State -> Expression -> Integer
eval state expr = undefined

-- Задание 3 -----------------------------------------

-- Следующий тип задаёт упрощённый набор инструкций (промежуточный язык Simpler).
data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

-- Переведите программу в DietStatement, сохранив её поведение.
desugar :: Statement -> DietStatement
desugar = undefined

-- Задание 4 -----------------------------------------

-- принимает начальное состояние и программу Simpler
-- и возвращает состояние после работы программы
runSimpler :: State -> DietStatement -> State
runSimpler = undefined

-- принимает начальное состояние и программу Simple
-- и возвращает состояние после работы программы
run :: State -> Statement -> State
run = undefined

-- Программы -------------------------------------------

{- Вычисление факториала для In >= 0 (готовый пример)

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


-- Задание 5 -----------------------------------------

-- Запишите следующую программу с помощью конструкторов Statement.
{- Вычисление целой части квадратного корня для A >= 0.
   Результат записывается в B.

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = undefined

-- Задание 6 -----------------------------------------

-- Запишите следующую программу с помощью конструкторов Statement.
{- Вычисление числа Фибоначчи для In >= 0.
   Здесь F0 = F1 = 1; результат записывается в Out.

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = undefined

-- Задание 7 -----------------------------------------

-- Реализуйте вывод выражения в обычной математической записи.
-- Имена переменных выводятся без кавычек, константы -- как целые числа,
-- операции -- знаками +, -, *, /, >, >=, <, <= и ==.
-- Сохраните структуру выражения: можно заключать каждую бинарную операцию
-- в скобки, не пытаясь убирать лишние скобки по приоритетам.
-- Например:
-- renderExpression (Op (Var "A") Plus (Val 1)) == "(A + 1)"
renderExpression :: Expression -> String
renderExpression = undefined

-- Пока используется Show из deriving у Expression.
-- Когда renderExpression будет реализована, уберите Show из deriving
-- у Expression и раскомментируйте следующий экземпляр.
-- После этого GHCi будет автоматически использовать renderExpression.
-- instance Show Expression where
--     show = renderExpression

-- Задание 8 -----------------------------------------

-- Реализуйте вывод программы Simple в синтаксисе примеров выше:
-- присваивание через :=, увеличение на единицу через ++,
-- if (...) { ... } else { ... }, while (...) { ... },
-- for (инициализация; условие; шаг) { ... }.
-- Инструкции блока разделяйте точкой с запятой, пустой блок выводите как {},
-- а Skip -- как skip. Выражения выводите с помощью renderExpression.
-- Группировку инструкций должно быть видно, в том числе во вложенных if.
-- Пробелы, переносы строк и отступы выберите сами.
-- Например:
-- renderStatement (Assign "A" (Val 1)) == "A := 1"
renderStatement :: Statement -> String
renderStatement = undefined

-- Когда renderStatement будет реализована, уберите Show из deriving
-- у Statement и раскомментируйте следующий экземпляр.
-- instance Show Statement where
--     show = renderStatement

-- Задание 9 -----------------------------------------

-- Реализуйте аналогичный вывод программы Simpler.
-- Для DAssign, DIf, DWhile и DSkip используйте ту же запись,
-- что для соответствующих инструкций Simple. DSequence задаёт
-- последовательность инструкций; сохраняйте видимую группировку.
-- Выражения выводите с помощью renderExpression.
renderDietStatement :: DietStatement -> String
renderDietStatement = undefined

-- Когда renderDietStatement будет реализована, уберите Show из deriving
-- у DietStatement и раскомментируйте следующий экземпляр.
-- instance Show DietStatement where
--     show = renderDietStatement
