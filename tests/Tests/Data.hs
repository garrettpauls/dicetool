module Tests.Data
( TestData(..)
, testData
) where

import DiceTool.Types (BinOp(..), Statement(..), StmtVal(..))
import System.Random  (StdGen, mkStdGen)

data TestData =
     TestData { testName :: String
              , command :: String
              , parsed  :: Statement
              , randomGen :: StdGen
              , result :: String }

stdGen :: StdGen
stdGen = mkStdGen 7625597484987

testData :: [TestData]
testData =
  [ TestData
      { testName  = "d20"
      , command   = "d20"
      , parsed    = LastStatement $ Roll 1 20
      , randomGen = stdGen
      , result    = "20 = 20" }
  , TestData
      { testName  = "2d6"
      , command   = "2d6"
      , parsed    = LastStatement $ Roll 2 6
      , randomGen = stdGen
      , result    = "(2 + 1) = 3" }
  , TestData
      { testName  = "1d4-2"
      , command   = "1d4-2"
      , parsed    = Statement (Roll 1 4) Minus $ LastStatement (Constant 2)
      , randomGen = stdGen
      , result    = "4 - 2 = 2" }
  , TestData
      { testName  = "1-2+3"
      , command   = "1-2+3"
      , parsed    = Statement (Constant 1) Minus $ Statement (Constant 2) Plus $ LastStatement (Constant 3)
      , randomGen = stdGen
      , result    = "1 - 2 + 3 = 2" }
  ]

