-- Не забудьте добавить тесты.

module Poly where
import Data.List

-- Многочлены
-- a -- тип коэффициентов, список начинается со свободного члена.
-- Бонус: при решении следующих заданий подумайте, какие стали бы проще или
-- сложнее при обратном порядке коэффициентов (и добавьте комментарий).
newtype Poly a = Poly [a]

-- Задание 1 -----------------------------------------

-- Определите многочлен $x$.
xPows x = map (\n -> x**n) [0..]
getCoefs (Poly p) = p
discardZeroes [] = []
discardZeroes p = if (last p /= 0) then p 
                  else discardZeroes $ init p
makePoly p x = sum $ zipWith (*) (getCoefs p) (xPows x)
--x :: Num a => Poly a
x p = Poly [0, 1]
-- Задание 2 -----------------------------------------

-- Функция, считающая значение многочлена в точке
--applyPoly :: Num a => Poly a -> a -> a
applyPoly p x = makePoly p x

-- Задание 3 ----------------------------------------

-- Определите равенство многочленов
-- Заметьте, что многочлены с разными списками коэффициентов
-- могут быть равны! Подумайте, почему.
-- Ответ: при вычислении суммы или разности многочленов, коэффициенты, стоящие при некоторых степенях, могут оказаться равными нулю. Наличие  таких нулей не меняет сам многочлен, но допускает разные списки коэффициентов. В данном случае нулевые элементы в конце списка могут быть отброшены
instance (Num a, Eq a) => Eq (Poly a) where
    (Poly a) == (Poly b) = discardZeroes a == discardZeroes b
 
-- Задание 4 -----------------------------------------
showPoly [] = show 0 
showPoly p =  let cOs = zip p [0..]
                  nonZeroCOs = filter (\(c,_) -> c /= 0) cOs
                  cShow c = if c == 1 then "" else show c ++ " *"
                  nShow n = case n of 
                              0 -> ""
                              1 -> "x" 
                              m -> "x^" ++ show m
                  cnShow c n = if c == 1 && n == 0 then show 1 
                               else intercalate " " $ filter (/="") [cShow c, nShow n]            
                  terms = map (\(c,n) -> cnShow c n) nonZeroCOs
              in intercalate " + " (reverse terms) 

-- Определите перевод многочлена в строку. 
-- Это должна быть стандартная математическая запись, 
-- например: show (3 * x * x + 1) == "3 * x^2 + 1").
-- (* и + для многочленов можно будет использовать после задания 6.)
--instance (Num a, Eq a, Show a) => Show (Poly a) where
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (Poly []) = show 0
    show (Poly p) = showPoly p
-- Задание 5 -----------------------------------------

-- Определите сложение многочленов
plus :: Num a => Poly a -> Poly a -> Poly a
plus p1 p2 = if (length (getCoefs p1) >= length (getCoefs p2)) then Poly $ zipWith (+) (getCoefs p1) ((getCoefs p2) ++ repeat 0)
             else plus p2 p1

-- Задание 6 -----------------------------------------

-- Определите умножение многочленов
multiplyBy a p1 = Poly (map (a*)(getCoefs p1))
multiplyByX p = Poly (0:coefs)
                where coefs = getCoefs p
times :: Num a => Poly a -> Poly a -> Poly a
times (Poly []) p2 = Poly []
times p1 p2 = let pTimesP2 = multiplyBy (head $ getCoefs p1) p2
                  xTimesP1Timesp2 = multiplyByX $ times (Poly $ tail $ getCoefs p1) p2
               in plus pTimesP2 xTimesP1Timesp2    

-- Задание 7 -----------------------------------------

-- Сделайте многочлены числовым типом
negatePoly p = Poly $ map Prelude.negate (getCoefs p)
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = negatePoly
    fromInteger a = Poly [fromIntegral a]
    -- Эти функции оставить как undefined, поскольку для 
    -- многочленов они не имеют математического смысла
    abs    = undefined
    signum = undefined

-- Задание 8 -----------------------------------------
--deriv (Poly []) = (Poly [])
--deriv (Poly (_:ps)) = Poly $ zipWith (*) ps [1..]
--nderiv n p | n == 0 = p
--           | n == 1 = deriv p
--           | otherwise = nderiv (n-1) (deriv p)


-- Реализуйте nderiv через deriv
class Num a => Differentiable a where
    -- взятие производной
    deriv  :: a -> a
    -- взятие n-ной производной
    nderiv :: Int -> a -> a
    nderiv n p | n == 0 = p
	           | otherwise = nderiv (n - 1) (deriv p)

-- Задание 9 -----------------------------------------
-- Определите экземпляр класса типов
deriv' (Poly []) = (Poly [])
deriv' (Poly (_:ps)) = Poly $ zipWith (*) ps [1..]
instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (Poly p)= deriv' (Poly p)
