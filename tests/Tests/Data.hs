module Tests.Data
( TestData(..)
, testData
) where

import DiceTool.Types (BinOp(..), MinMax(..), Statement(..), StmtVal(..))
import System.Random  (StdGen, mkStdGen)

data TestData =
     TestData { command :: String
              , parsed  :: Statement
              , randomGen :: StdGen
              , result :: String }

stdGen :: StdGen
stdGen = mkStdGen 7625597484987

testData :: [TestData]
testData =
  [ TestData
      { command   = "d20"
      , parsed    = LastStatement $ Roll 1 20
      , randomGen = stdGen
      , result    = "20 = 20" }
  , TestData
      { command   = "2d6"
      , parsed    = LastStatement $ Roll 2 6
      , randomGen = stdGen
      , result    = "(2 + 1) = 3" }
  , TestData
      { command   = "1d4-2"
      , parsed    = Statement (Roll 1 4) Minus $ LastStatement (Constant 2)
      , randomGen = stdGen
      , result    = "4 - 2 = 2" }
  , TestData
      { command   = "1-2+3"
      , parsed    = Statement (Constant 1) Minus $ Statement (Constant 2) Plus $ LastStatement (Constant 3)
      , randomGen = stdGen
      , result    = "1 - 2 + 3 = 2" }
  , TestData
      { command   = "5d6"
      , parsed    = LastStatement $ Roll 5 6
      , randomGen = stdGen
      , result    = "(2 + 1 + 2 + 3 + 3) = 11"
      }
  , TestData
      { command   = "5d6 take max 3"
      , parsed    = LastStatement $ RollTake 5 6 Max 3
      , randomGen = stdGen
      , result    = "(3 + 3 + 2) = 8" }
  , TestData
      { command   = "5d6 take min 3"
      , parsed    = LastStatement $ RollTake 5 6 Min 3
      , randomGen = stdGen
      , result    = "(1 + 2 + 2) = 5" }
  , TestData
      { command   = "5d6 take max 2+3"
      , parsed    = Statement (RollTake 5 6 Max 2) Plus $ LastStatement (Constant 3)
      , randomGen = stdGen
      , result    = "(3 + 3) + 3 = 9" }
  ]

{- TestData template:
  , TestData
      { command   = ""
      , parsed    = Statement
      , randomGen = stdGen
      , result    = "" }
-}
