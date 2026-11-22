{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
module PickExercise where

-- Proof — это (). Документация комбинаторов приведена в комментариях модуля:
-- https://github.com/ucsd-progsys/liquidhaskell/blob/63912f2fc10abcd36ec95e08f7609ef3d65cae32/liquid-prelude/src/Language/Haskell/Liquid/ProofCombinators.hs
import Language.Haskell.Liquid.ProofCombinators (Proof)
import Pick (pick)

{-@ reflect opposite @-}
opposite :: (a -> Bool) -> a -> Bool
opposite p x = not (p x)

-- Суммарное число выбранных и пропущенных элементов равно длине списка.
{-@ partitionLength :: p:(a -> Bool) -> xs:[a]
      -> {len (pick p xs) + len (pick (opposite p) xs) == len xs} @-}
partitionLength :: (a -> Bool) -> [a] -> Proof
partitionLength = _
