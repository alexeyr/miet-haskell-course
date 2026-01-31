module SimpleLang where
-- Язык Simple -- очень простой императивный язык.
-- В нём только один тип данных: целые числа.

data Expression =
    Var String                   -- Переменные
  | Val Int                      -- Целые константы
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
  | For      Statement  Expression Statement Statement
    -- как { ... } в C-подобных языках
  | Block [Statement]
    -- пустая инструкция
  | Skip
  deriving (Show, Eq)

-- примеры программ на этом языке в конце модуля

-- по состоянию можно получить значение каждой переменной
-- (в реальной программе скорее использовалось бы Data.Map.Map String Int)
type State = String -> Int

-- Задание 1 -----------------------------------------

-- в начальном состоянии все переменные имеют значение 0
empty :: State
empty _ = 0

-- возвращает состояние, в котором переменная var имеет значение newVal, 
-- все остальные -- то же, что в state
extend :: State -> String -> Int -> State
extend state var newVal x
| x == var = newVal
| otherwise = state x
-- Задание 2 -----------------------------------------

-- возвращает значение выражения expr при значениях переменных из state.
eval :: State -> Expression -> Int
eval state expr = case expr of
Var x -> state x
Val n -> n
Op e1 op e2 ->
let v1 = eval state e1
    v2 = eval state e2
in case op of
Plus -> v1 + v2
Minus -> v1 - v2
Times -> v1 * v2
Divide -> v1 `div` v2
Gt -> if v1 >  v2 then 1 else 0
Ge-> if v1 >= v2 then 1 else 0
Lt -> if v1 <  v2 then 1 else 0
Le -> if v1 <= v2 then 1 else 0
Eql -> if v1 == v2 then 1 else 0
-- Задание 3 -----------------------------------------

-- Можно выразить Incr через Assign, For через While, Block через 
-- последовательное выполнение двух инструкций (; в C).
-- Следующий тип задаёт упрощённый набор инструкций (промежуточный язык Simpler).
data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

seq2 :: DietStatement -> DietStatement -> DietStatement
seq2 DSkip s = s
seq2 s DSkip = s
seq2 s1 s2   = DSequence s1 s2
-- упрощает программу Simple
desugar :: Statement -> DietStatement
desugar stmt =
  case stmt of
    Skip -> DSkip

    Assign x e ->
      DAssign x e

    Incr x ->
      DAssign x (Op (Var x) Plus (Val 1))

    Block [] ->
      DSkip

    Block (s:ss) ->
      seq2 (desugar s) (desugar (Block ss))

    If e s1 s2 ->
      DIf e (desugar s1) (desugar s2)

    While e s ->
      DWhile e (desugar s)

    For init cond step body ->
      seq2 (desugar init)
           (DWhile cond
             (seq2 (desugar body) (desugar step)))

-- Задание 4 -----------------------------------------

-- принимает начальное состояние и программу Simpler
-- и возвращает состояние после работы программы

runSimpler :: State -> DietStatement -> State
runSimpler state stmt =
  case stmt of
    DSkip ->
      state

    DAssign x e ->
      extend state x (eval state e)

    DSequence s1 s2 ->
      let state' = runSimpler state s1
      in runSimpler state' s2

    DIf e s1 s2 ->
      if eval state e /= 0
        then runSimpler state s1
        else runSimpler state s2

    DWhile e s ->
      if eval state e /= 0
        then runSimpler (runSimpler state s) stmt
        else state

-- 
-- in s "A" ~?= 10

-- принимает начальное состояние и программу Simple
-- и возвращает состояние после работы программы
run :: State -> Statement -> State
run state stmt = runSimpler state (desugar stmt)

-- Программы -------------------------------------------

{- Вычисление факториала

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Вычисление целой части квадратного корня

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot =
  Block
    [ Assign "B" (Val 0)
    , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
        (Incr "B")
    , Assign "B" (Op (Var "B") Minus (Val 1))
    ]
{- Вычисление числа Фибоначчи

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
fibonacci = Block
    [ Assign "F0" (Val 1)
    , Assign "F1" (Val 1)
    , If (Op (Var "In") Eql (Val 0))
        (Assign "Out" (Var "F0"))
        (If (Op (Var "In") Eql (Val 1))
            (Assign "Out" (Var "F1"))
            (For (Assign "C" (Val 2))
                 (Op (Var "C") Le (Var "In"))
                 (Incr "C")
                 (Block
                   [ Assign "T"  (Op (Var "F0") Plus (Var "F1"))
                   , Assign "F0" (Var "F1")
                   , Assign "F1" (Var "T")
                   , Assign "Out" (Var "T")
                   ])))
    ]
