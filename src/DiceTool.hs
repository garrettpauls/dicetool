module DiceTool
( diceTool
) where

import Data.Version         (showVersion)
import DiceTool.Commands    (Command(..), parseCommand)
import DiceTool.IO.Strict   (putStr, putStrLn)
import DiceTool.Parser      (parseStatement)
import DiceTool.Interpreter (interpret)
import Paths_dicetool       (version)
import Prelude       hiding (putStr, putStrLn)
import System.Random        (RandomGen, newStdGen)

diceTool :: IO ()
diceTool = do
  g <- newStdGen
  repl g Help

repl :: RandomGen g => g -> Command -> IO ()
repl g lastCmd = do
  putStr "> "
  input <- getLine
  let command = parseCommand lastCmd input
      (result, fg) = runCommand g command
  putStrLn result
  case command of
    Exit -> return ()
    _    -> repl fg command

runCommand :: RandomGen g => g -> Command -> (String, g)
runCommand g cmd = case cmd of
  Exit     -> ("", g)
  Help     -> (helpMessage, g)
  Version  -> (versionMessage, g)
  Script x -> evalScript g x

helpMessage :: String
helpMessage = "TODO: Write help message"

versionMessage :: String
versionMessage = "dicetool " ++ showVersion version

evalScript :: RandomGen g => g -> String -> (String, g)
evalScript g inp = case parseStatement inp of
  Left err -> (err, g)
  Right st -> interpret g st

