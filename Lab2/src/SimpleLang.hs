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
empty = const 0


-- возвращает состояние, в котором переменная var имеет значение newVal, 
-- все остальные -- то же, что в state
extend :: State -> String -> Int -> State
extend state var newVal v = if v == var then newVal else state v 


-- Задание 2 -----------------------------------------


-- возвращает значение выражения expr при значениях переменных из state.
eval :: State -> Expression -> Int
eval state (Var vr) = state vr
eval _ (Val vl) = vl
eval state (Op vl1 operation vl2) = case operation of
      Plus -> var1 + var2
      Minus -> var1 - var2
      Times -> var1 * var2
      Divide -> var1 `div` var2
      Gt -> if var1 > var2 then 1 else 0
      Ge -> if var1 >= var2 then 1 else 0
      Lt -> if var1 < var2 then 1 else 0
      Le -> if var1 <= var2 then 1 else 0
      Eql -> if var1 == var2 then 1 else 0
      where
        var1 = eval state vl1
        var2 = eval state vl2  

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


-- упрощает программу Simple
desugar :: Statement -> DietStatement
-- If
desugar (If expr outTrue outFalse) = DIf expr (desugar outTrue) (desugar outFalse) 
-- While
desugar (While expr state) = DWhile expr (desugar state) 
-- Incr
desugar (Assign var expr) = DAssign var expr 
desugar (Incr var) = DAssign var (Op (Var var) Plus (Val 1))
-- SKip
desugar Skip = DSkip -- ????
-- Block
desugar (Block []) = DSkip
desugar (Block (st : sts)) = DSequence (desugar st) (desugar (Block sts))
-- For
desugar (For state expr state1 state2) = DSequence (desugar state) (DWhile expr (DSequence (desugar state2) (desugar state1))) 

-- Задание 4 -----------------------------------------

-- принимает начальное состояние и программу Simpler
-- и возвращает состояние после работы программы
runSimpler :: State -> DietStatement -> State

runSimpler state (DAssign var expr) = extend state var (eval state expr) 

runSimpler state (DIf expr out1 out2) =
    if eval state expr == 1 then runSimpler state out1 else runSimpler state out2

runSimpler state (DWhile exp st) = 
  if eval state exp == 1 then runSimpler (runSimpler state st) (DWhile exp st) else state

runSimpler state (DSequence st1 st2) = runSimpler (runSimpler state st1) st2  

runSimpler state DSkip = state

-- 
-- in s "A" ~?= 10

-- принимает начальное состояние и программу Simple
-- и возвращает состояние после работы программы
run :: State -> Statement -> State
run state (Assign var expr) = extend state var (eval state expr) 
run state (Incr var) = extend state var (state var + 1)  
run state (If cond stmt1 stmt2) =
    if eval state cond /= 0 then run state stmt1 else run state stmt2  
run state (While cond stmt) =
    let loop st = if eval st cond /= 0 then loop (run st stmt) else st
    in loop state  
run state (For initStmt cond incrStmt bodyStmt) =
    let initState = run state initStmt
        loop st = if eval st cond /= 0
                 then loop (run st (Block [bodyStmt, incrStmt]))
                 else st
    in loop initState 
run state (Block stmts) = foldl run state stmts  
run state Skip = state 

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
  Block [ Assign "B" (Val 0)
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
fibonacci = 
  Block [
    Assign "F0" (Val 1),
    Assign "F1" (Val 1),
    If (Op (Var "In") Eql (Val 0))
      (Assign "Out" (Val 1))
      (If (Op (Var "In") Eql (Val 1))
        (Assign "Out" (Var "F0"))
        (For (Assign "C" (Val 2))
          (Op (Var "C") Le (Var "In"))
          (Incr "C")
          (Block [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
            , Assign "F0" (Var "F1")
            , Assign "F1" (Var "T")
            , Assign "Out" (Var "T")
            ]
          )
        )
    )
  ]
