module DiceTool.Types
( BinOp(..)
, MinMax(..)
, StmtVal(..)
, Statement(..)
) where

data BinOp = Plus | Minus deriving (Eq)
instance Show BinOp where
  show Plus  = "+"
  show Minus = "-"

data MinMax = Min | Max deriving (Eq, Show)

data StmtVal =
     Constant Int
   | Roll Int Int
   | RollTake Int Int MinMax Int
   deriving (Eq, Show)

data Statement =
     Statement StmtVal BinOp Statement
   | LastStatement StmtVal
   deriving (Eq, Show)

