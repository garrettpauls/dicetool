module Main(main) where

import Tests.Parser      (parserTests)
import Tests.Interpreter (interpreterTests)

import Test.Framework (Test, defaultMain)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = parserTests ++ interpreterTests

