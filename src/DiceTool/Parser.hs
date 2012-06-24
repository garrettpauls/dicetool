module DiceTool.Parser
( parseStatement
) where

import Control.Applicative ((<$))
import Control.Monad       (liftM)
import DiceTool.Types      (BinOp(..), MinMax(..), StmtVal(..), Statement(..))
import Prelude     hiding  (const)
import Text.Parsec         (char, digit, many, many1, oneOf, optionMaybe
                           ,parse, skipMany, skipMany1, string, try
                           ,(<|>))
import Text.Parsec.String  (Parser)

parseStatement :: String -> Either String Statement
parseStatement input = case parse statement "dicetool" input of
  Left err -> Left $ show err
  Right x  -> Right x

statement :: Parser Statement
statement = do
  first <- stmtVal
  rest  <- many stmtVal'
  return $ mkStatement first rest
  where
    stmtVal' :: Parser (BinOp, StmtVal)
    stmtVal' = do
      op   <- binOp
      stmt <- stmtVal
      return (op, stmt)
    mkStatement :: StmtVal -> [(BinOp, StmtVal)] -> Statement
    mkStatement x [] = LastStatement x
    mkStatement x ((so, st):rest) =
      let restStmt = mkStatement st rest
       in Statement x so restStmt

binOp :: Parser BinOp
binOp = do
  optWhitespace
  op <- oneOf "+-"
  optWhitespace
  return $ case op of
    '-' -> Minus
    _   -> Plus

stmtVal :: Parser StmtVal
stmtVal = do
  mN <- optionMaybe parseInt
  mD <- optionMaybe $ char 'd'
  case (mN, mD) of
    (Just n, Nothing)  -> return $ Constant n
    (Just n, Just _)   -> dieRoll n
    (Nothing, Just _)  -> dieRoll 1
    (Nothing, Nothing) -> fail "Expected a number or 'd'"
  where
    dieRoll :: Int -> Parser StmtVal
    dieRoll n = do
      s <- parseInt
      mTake <- optionMaybe parseTake
      return $ case mTake of
        (Just (m, t))  -> RollTake n s m t
        (Nothing)      -> Roll n s

parseTake :: Parser (MinMax, Int)
parseTake = do
  whitespace
  _ <- string "take"
  whitespace
  minMax <- ( Min <$ try (string "min")
          <|> Max <$ string "max")
  whitespace
  n <- parseInt
  return (minMax, n)

parseInt :: Parser Int
parseInt = liftM read $ many1 digit

optWhitespace :: Parser ()
optWhitespace = skipMany $ oneOf whitespaceChars

whitespace :: Parser ()
whitespace = skipMany1 $ oneOf whitespaceChars

whitespaceChars :: [Char]
whitespaceChars = " \t"

