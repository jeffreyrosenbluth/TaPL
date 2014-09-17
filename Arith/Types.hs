-- | Utilities for Arithmetic Expressions
--   TaPL Chapter 4
--   (c) 2014 Jeffrey Rosenbluth

module Types where

import Text.Parsec.String

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

type ParserTok = GenParser Token ()

data Term =
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term

instance Show Term where
  show t
    | isval t = case t of
        TmTrue       -> "true"
        TmFalse      -> "false"
        TmZero       -> "0"
        s@(TmSucc a) -> show . toInt $ s
          where
            toInt (TmSucc TmZero) = 1
            toInt (TmSucc x) = 1 + toInt x 
    | otherwise = "Can only show value terms"

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
