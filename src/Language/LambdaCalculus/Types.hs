module Language.LambdaCalculus.Types
  ( Ty(..)
  ) where

data Ty =
    TyBool
  | TyArr Ty Ty
  deriving (Eq, Show)
