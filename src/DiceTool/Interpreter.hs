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
groupValue = opListValue.groupsToOpList

groupsToOpList :: Group -> ([Int], [BinOp])
groupsToOpList (Group xs op next) =
  let (rv, rop) = groupsToOpList next
   in (sum xs:rv, op:rop)
groupsToOpList (LastGroup xs) = ([sum xs], [])

opListValue :: ([Int], [BinOp]) -> Int
opListValue ([], _) = 0
opListValue ([x],_) = x
opListValue (xs,[]) = sum xs
opListValue ((x:y:rest),(op:ops)) =
  let z = applyOp op x y
   in opListValue (z:rest, ops)

applyOp :: BinOp -> Int -> Int -> Int
applyOp Plus  x y = x + y
applyOp Minus x y = x - y

eval :: RandomGen g => g -> StmtVal -> ([Int], g)
eval g (Constant c) = ([c], g)
eval g (Roll n s)   = randomRCs (1, s) n g

