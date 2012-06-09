module DiceTool.Commands
( Command(..)
, parseCommand
) where

import Data.Char (toLower)

data Command =
     Exit
   | Help
   | Version
   | Script String

parseCommand :: Command -> String -> Command
parseCommand lastCmd cmd = case map toLower cmd of
  "" -> lastCmd
  "exit" -> Exit
  "help" -> Help
  "version" -> Version
  lCmd -> Script lCmd

