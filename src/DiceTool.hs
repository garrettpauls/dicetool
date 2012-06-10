module DiceTool
( diceTool
) where

import Data.Version       (showVersion)
import DiceTool.Commands  (Command(..), parseCommand)
import DiceTool.IO.Strict (putStr, putStrLn)
import DiceTool.Parser    (parseStatement)
import Paths_dicetool     (version)
import Prelude     hiding (putStr, putStrLn)

diceTool :: IO ()
diceTool = repl Help

repl :: Command -> IO ()
repl lastCmd = do
  putStr "> "
  input <- getLine
  let command = parseCommand lastCmd input
      result  = runCommand command
  putStrLn result
  case command of
    Exit -> return ()
    _    -> repl command

runCommand :: Command -> String
runCommand cmd = case cmd of
  Exit     -> ""
  Help     -> helpMessage
  Version  -> versionMessage
  Script x -> evalScript x

helpMessage :: String
helpMessage = "TODO: Write help message"

versionMessage :: String
versionMessage = "dicetool " ++ showVersion version

evalScript :: String -> String
evalScript inp = case parseStatement inp of
  Left err -> err
  Right st -> show st

