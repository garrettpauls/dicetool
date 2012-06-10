module DiceTool.Random
( randomRCs
) where

import System.Random (Random, RandomGen, randomR)

randomRCs :: (Random a, RandomGen g) => (a, a) -> Int -> g -> ([a], g)
randomRCs r n g
  | n <= 0    = ([], g)
  | otherwise =
    let (x, ng)    = randomR r g
        (rest, fg) = randomRCs r (n-1) ng
     in (x:rest, fg)
