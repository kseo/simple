module Language.LambdaCalculus.Pretty.Term
  ( printTm
  ) where

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Context

printTm :: Context -> Term -> String
printTm ctx t = case t of
  TmAbs _ x ty t1 -> let
      (ctx', x') = pickFreshName ctx x
    in "(\\" ++ x' ++ ":" ++ show ty ++ "." ++ printTm ctx' t1 ++ ")"
  TmApp _ t1 t2 ->
    "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
  TmVar fi x n ->
    if ctxLength ctx == n then
      indexToName fi ctx x
    else
      "[bad index]"
  TmIf _ t1 t2 t3 ->
    "if" ++ " " ++ printTm ctx t1 ++ " then " ++ printTm ctx t2 ++ " else " ++ printTm ctx t3
  TmTrue _ -> "true"
  TmFalse _ -> "false"
