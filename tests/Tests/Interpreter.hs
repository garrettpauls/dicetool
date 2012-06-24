module Tests.Interpreter
( interpreterTests
) where

import DiceTool.Interpreter (interpret)
import Tests.Data           (TestData(TestData), testData)

import Test.HUnit     ((@?=))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

interpreterTests :: [Test]
interpreterTests =
  [ testGroup "Point tests DiceTool.Interpreter" $ map mkTestCase testData ]

mkTestCase :: TestData -> Test
mkTestCase (TestData command stmt g result) =
  testCase ("Interpret " ++ command) $ (fst $ interpret g stmt) @?= result

