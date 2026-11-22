{-@ LIQUID "--reflection" @-}
module Pick (pick) where

{-@ reflect pick @-}
pick :: (a -> Bool) -> [a] -> [a]
pick _ [] = []
pick p (x:xs)
  | p x       = x : pick p xs
  | otherwise = pick p xs
