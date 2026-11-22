{-@ LIQUID "--exact-data-cons" @-}
module LineRange (LineRange(..), mkRange) where

-- Обе границы включены. Один номер n представляется как LineRange n n.
{-@ data LineRange = LineRange { lo :: {n:Integer | 1 <= n}
                               , hi :: {n:Integer | lo <= n} } @-}
data LineRange = LineRange Integer Integer
  deriving (Eq, Show)

-- Задание 1. Верните Just с указанными границами, если они допустимы,
-- иначе Nothing.
-- Необязательно: уточните тип результата в зависимости от обеих границ.
mkRange :: Integer -> Integer -> Maybe LineRange
mkRange _ _ = error "Реализуйте mkRange"
