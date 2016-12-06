module Language.LambdaCalculus.AST.Term
  ( Info(..)
  , Term(..)
  ) where

import Language.LambdaCalculus.Types

data Term =
    TmVar Info Int Int
  | TmAbs Info String Ty Term
  | TmApp Info Term Term
  | TmIf Info Term Term Term
  | TmTrue Info
  | TmFalse Info
  deriving (Show)

data Info = Info { row :: Int, col :: Int }

instance Show Info where
  show (Info r c) = "(" ++ show r ++ "," ++ show c ++ ")"
