module Language.LambdaCalculus.Pretty.Types
  ( printType
  ) where

import Language.LambdaCalculus.Types

printType :: Ty -> String
printType TyBool = "Bool"
printType (TyArr t1 t2) = "(" ++ printType t1 ++ "->" ++ printType t2 ++ ")"
