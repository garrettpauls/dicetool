module Tests.Parser
( parserTests
) where

import DiceTool.Parser (parseStatement)
import Tests.Data      (TestData(TestData), testData)

import Test.HUnit      ((@?=))
import Test.Framework  (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

parserTests :: [Test]
parserTests =
  [ testGroup "Point tests DiceTool.Parser" $ map mkTestCase testData ]

mkTestCase :: TestData -> Test
mkTestCase (TestData command parsed _ _) =
  testCase ("Parse " ++ command) $ parseStatement command @?= (Right parsed)

