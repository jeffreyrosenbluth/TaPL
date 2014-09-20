-- | Lexer for Arithmetic Expressions
--   TaPL Chapter 4
--   (c) 2014 Jeffrey Rosenbluth

module Lex 
  ( runLexer
  ) where

import Control.Applicative ((<$), (<*))
import Text.Parsec
import Text.Parsec.String
import Types

keywords :: [(Token, String)]
keywords = [ (TokTrue, "true"), (TokFalse, "false"), (TokIf, "if")
           , (TokThen, "then"), (TokElse, "else")  , (TokZero, "0")
           , (TokSucc, "succ"), (TokPred, "pred")  , (TokIsZero, "iszero")]

lexer :: Parser Token
lexer = foldr1 (<|>) (map (try . uncurry tokenParser) keywords)
  where
    tokenParser t w = t <$ wordParser w
    wordParser s = string s <* (skipMany1 (space <|> char '(' <|> char ')') <|> eof)

runLexer :: String -> Either ParseError [Token]
runLexer = parse (many lexer) ""
