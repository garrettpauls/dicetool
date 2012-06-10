module DiceTool.Types
( BinOp(..)
, StmtVal(..)
, Statement(..)
) where

data BinOp = Plus | Minus deriving (Eq)
instance Show BinOp where
  show Plus  = "+"
  show Minus = "-"

data StmtVal =
     Constant Int
   | Roll Int Int
   deriving (Eq, Show)

data Statement =
     Statement StmtVal BinOp Statement
   | LastStatement StmtVal
   deriving (Eq, Show)

