module Tests.Parser
( parserTests
) where

import DiceTool.Types  (BinOp(..), Statement(..), StmtVal(..))
import DiceTool.Parser (parseStatement)

import Test.HUnit hiding (Test)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)


parserTests :: [Test]
parserTests =
  [ testGroup "Point tests DiceTool.Parser"
    [ testCase "d20" $ parseStatement "d20" @?= (Right $ LastStatement $ Roll 1 20)
    , testCase "2d6" $ parseStatement "2d6" @?= (Right $ LastStatement $ Roll 2 6)
    , testCase "1d4-2" $ parseStatement "1d4-2" @?= (Right $ Statement (Roll 1 4) Minus $ LastStatement $ Constant 2)
    , testCase "1-2+3" $ parseStatement "1-2+3" @?= (Right $ Statement (Constant 1) Minus $ Statement (Constant 2) Plus $ LastStatement $ Constant 3)
    ]
  ]
