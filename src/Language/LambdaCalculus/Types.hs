module Language.LambdaCalculus.Types
  ( Ty(..)
  ) where

data Ty =
    TyBool
  | TyArr Ty Ty
  deriving (Eq)

instance Show Ty where
  show TyBool = "Bool"
  show (TyArr t1 t2) = "(" ++ show t1 ++ "->" ++ show t2 ++ ")"
