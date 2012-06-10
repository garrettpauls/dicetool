module DiceTool.Types
( BinOp(..)
, StmtVal(..)
, Statement(..)
) where

data BinOp = Plus | Minus
instance Show BinOp where
  show Plus  = "+"
  show Minus = "-"

data StmtVal =
     Constant Int
   | Roll Int Int
   deriving Show

data Statement =
     Statement StmtVal BinOp Statement
   | LastStatement StmtVal
   deriving Show

