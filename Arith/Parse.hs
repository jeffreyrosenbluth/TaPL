-- | Parser for Arithmetic Expressions
--   TaPL Chapter 4
--   (c) 2014 Jeffrey Rosenbluth

module Parse
  ( runTokParser
  ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Text.Parsec
import Text.Parsec.Pos

import Lex
import Types

-- We are not going to use source positions so we create a dummy pos
-- allowing us to use the 'token' function.
nullPos :: a -> SourcePos
nullPos = const $ initialPos ""

-- Make a parser for a specific token.
literal :: Token -> Term -> ParserTok Term
literal tok tm = token show nullPos (\x -> if x == tok then Just tm else Nothing)

-- Make a parser that skips a specific token.
skip :: Token -> ParserTok ()
skip tok = token show nullPos (\x -> if x == tok then Just () else Nothing)

true, false, zero :: ParserTok Term
true  = literal TokTrue  TmTrue
false = literal TokFalse TmFalse
zero  = literal TokZero  TmZero

ifTerm :: ParserTok Term
ifTerm = TmIf <$> (skip TokIf   *> term) 
              <*> (skip TokThen *> term) 
              <*> (skip TokElse *> term)

succTerm, predTerm, iszero :: ParserTok Term
succTerm = TmSucc   <$> (skip TokSucc   *> term)
predTerm = TmPred   <$> (skip TokPred   *> term)
iszero   = TmIsZero <$> (skip TokIsZero *> term)

term :: ParserTok Term
term = ifTerm <|> iszero
              <|> succTerm
              <|> predTerm
              <|> true
              <|> false
              <|> zero

runTokParser :: String -> Either ParseError Term
runTokParser s = parse term [] toks
  where
    toks = case runLexer s of
      Right t -> t
      Left _ -> error "Syntax Error"
