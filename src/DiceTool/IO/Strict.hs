module DiceTool.IO.Strict
( putStr
, putStrLn
) where

import Prelude hiding (putStr, putStrLn)
import System.IO (hFlush, hPutStr, hPutStrLn, stdout)

putStr :: String -> IO ()
putStr x = do
  hPutStr stdout x
  hFlush  stdout

putStrLn :: String -> IO ()
putStrLn x = do
  hPutStrLn stdout x
  hFlush    stdout

