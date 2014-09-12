module Arith where

import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>))
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String
import Text.Parsec.Char

-- | Lexing -------------------------------------------------------------------
--
data Token =
    TokTrue
  | TokFalse
  | TokIf
  | TokThen
  | TokElse
  | TokZero
  | TokSucc
  | TokPred
  | TokIsZero
  | TokLParen
  | TokRParen
    deriving (Show, Eq)

keywords :: [(Token, String)]
keywords = [ (TokTrue, "true"), (TokFalse, "false"), (TokIf, "if")
           , (TokThen, "then"), (TokElse, "else")  , (TokZero, "0")
           , (TokSucc, "succ"), (TokPred, "pred")  , (TokIsZero, "iszero")
           , (TokLParen, "(") , (TokRParen, ")") ]

lexer = foldr1 (<|>) (map (try . uncurry tokenParser) keywords)
  where
    tokenParser t w = t <$ wordParser w
    wordParser s = string s <* (skipMany1 space <|> eof)

-- Make sure a left paren is followed by a space and a right paren is 
-- preceded by a space.
prepare :: String -> String
prepare []    = []
prepare (x:xs)
  | x == '('  = "( " ++ prepare xs
  | x == ')'  = " )" ++ prepare xs
  | otherwise = x : prepare xs

runLexer :: String -> Either ParseError [Token]
runLexer = parse (many lexer) "" . prepare

-- | Parsing ------------------------------------------------------------------
--
data Term =
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
    deriving (Show)

type ParserTok = GenParser Token ()

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

paren :: ParserTok Term
paren = skip TokLParen *> term <* skip TokRParen

term :: ParserTok Term
term = paren <|> ifTerm
             <|> iszero
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
      Left e -> error "Syntax Error"

-- | Interpreter --------------------------------------------------------------
-- 
isnumerical :: Term -> Bool
isnumerical TmZero     = True
isnumerical (TmSucc t) = isnumerical t
isnumerical _          = False

isval :: Term -> Bool
isval TmTrue  = True
isval TmFalse = True
isval t
  | isnumerical t = True
  | otherwise     = False


eval1 :: Term -> Term
eval1 (TmIf TmTrue t2 _)  = t2
eval1 (TmIf TmFalse _ t3) = t3
eval1 (TmIf t1 t2 t3)     = TmIf (eval1 t1) t2 t3

eval1 (TmSucc t)          = TmSucc (eval1 t)

eval1 (TmPred TmZero)     = TmZero
eval1 (TmPred (TmSucc t))
  | isnumerical t         = t
eval1 (TmPred t)          = TmPred (eval1 t)

eval1 (TmIsZero TmZero)   = TmTrue
eval1 (TmIsZero (TmSucc t)) 
  | isnumerical t         = TmFalse
eval1 (TmIsZero t)        = TmIsZero (eval1 t)

eval1 _                   = error "No rules apply."

-- | In contrast to the OCaml code we stop the recursion when we reach
-- a value. This avoids the need to use Control.Exception and allows us
-- to staty out of the IO monad.
eval :: Term -> Term
eval t
  | isval t = t
  | otherwise = eval . eval1 $ t

runEval :: String -> Term
runEval s = eval t
  where
    t = case runTokParser s of
          Left _ -> error "The sky has fallen"
          Right t' -> t'

 
-- | Testing ------------------------------------------------------------------
--
test0 = "true"
test1 = "if false then true else false"
test2 = "0"
test3 = "if false then 0 else (succ 0)"
test4 = "iszero (pred (succ (succ 0)))"

