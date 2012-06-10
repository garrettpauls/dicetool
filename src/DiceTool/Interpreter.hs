module DiceTool.Interpreter
( interpret
) where

import Data.List       (intercalate)
import DiceTool.Random (randomRCs)
import DiceTool.Types  (BinOp(..), StmtVal(..), Statement(..))
import System.Random   (RandomGen)

data Group =
     Group [Int] BinOp Group
   | LastGroup [Int]

interpret :: RandomGen g => g -> Statement -> (String, g)
interpret g stmt =
  let (group, fg) = evalStatement g stmt
   in (showGroup group, fg)

evalStatement :: RandomGen g => g -> Statement -> (Group, g)
evalStatement g (Statement val op next) =
  let (vlist, ng) = eval g val
      (rest, fg)  = evalStatement ng next
   in (Group vlist op rest, fg)
evalStatement g (LastStatement val) =
  let (vlist, fg) = eval g val
   in (LastGroup vlist, fg)

showGroup :: Group -> String
showGroup g = showGroupLong g ++ " = " ++ (show $ groupValue g)

showGroupLong :: Group -> String
showGroupLong g = case g of
  (Group xs op next) -> intercalate " " [showValList xs, show op, showGroupLong next]
  (LastGroup xs)     -> showValList xs
  where
    showValList :: [Int] -> String
    showValList [x] = show x
    showValList xs  = "(" ++ (intercalate " + " $ map show xs) ++ ")"

groupValue :: Group -> Int
groupValue (Group xs Plus next)  = sum xs + groupValue next
groupValue (Group xs Minus next) = sum xs - groupValue next
groupValue (LastGroup xs) = sum xs

eval :: RandomGen g => g -> StmtVal -> ([Int], g)
eval g (Constant c) = ([c], g)
eval g (Roll n s)   = randomRCs (1, s) n g

