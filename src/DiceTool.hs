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
helpMessage =
  " Type the desired dice to roll at the prompt.\n\
  \ A dice roll is represented as follows:\n\
  \ <#1>d<#2>[ take <min/max> <#3>]\n\
  \ where #1 is the number of dice to roll\n\
  \ and #2 is the size of the dice to roll.\n\
  \ If only one die is rolled, #1 can be omitted.\n\
  \ \n\
  \ The take command is optional. #3 is the number of dice to take. \n\
  \ Selecting min means take the minimun #3 dice results, while \n\
  \ selecting max means take the maximum #3 dice results. \n\
  \ \n\
  \ Rolls can be added or subtracted with + and -.\n\
  \ You can also add or subtract constants by just typing the number.\n\
  \ \n\
  \ Example:\n\
  \   '1d20' rolls one 20-sided die. It can also be typed 'd20'.\n\
  \   '2d6' rolls two 6-sided dice.\n\
  \   '2d4 + 3' rolls two 4-sided dice and adds 3 to the result.\n\
  \   '2d6 + d4 - 2' rolls two 6-sided dice and a 4-sided die and subtracts 2 from the total.\n\
  \ Other commands are\n\
  \ \n\
  \ version - shows the program version.\n\
  \ exit - quits the program."

versionMessage :: String
versionMessage = "dicetool " ++ showVersion version

evalScript :: RandomGen g => g -> String -> (String, g)
evalScript g inp = case parseStatement inp of
  Left err -> (err, g)
  Right st -> interpret g st

