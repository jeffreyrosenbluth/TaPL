-- | Evaluator for Arithmetic Expressions
--   TaPL Chapter 4
--   (c) 2014 Jeffrey Rosenbluth

module Eval where

import Parse
import Types

-- | Interpreter --------------------------------------------------------------
 
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
runEval s
  | Right t <- runTokParser s = eval t
  | otherwise = error "The sky has fallen"

-- | Big step semantics ------------------------------------------------------
--   Exercise 4.2.2

eval1' :: Term -> Term
eval1' t | isval t = t

eval1' (TmSucc t) = case eval1' t of
  t' | isnumerical t' -> TmSucc t'

eval1' (TmPred t) = case eval1' t of
  TmZero                       -> TmZero
  (TmSucc t') | isnumerical t' -> t'

eval1' (TmIf t1 t2 t3) = case (eval1' t1, eval1' t2, eval1' t3) of
  (TmTrue, t, t') | isval t   -> t
  (TmFalse, t, t') | isval t' -> t'

eval1' (TmIsZero t) = case eval1' t of
  TmZero                       -> TmTrue
  (TmSucc t') | isnumerical t' -> TmFalse
  
runEval' :: String -> Term
runEval' s
  | Right t <- runTokParser s = eval1' t
  | otherwise = error "The sky has fallen"
