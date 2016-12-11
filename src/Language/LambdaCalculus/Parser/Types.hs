module Language.LambdaCalculus.Parser.Types
  ( parseType
  ) where

import Language.LambdaCalculus.Parser.Common
import Language.LambdaCalculus.Types

import Text.Parsec

parseBoolType :: Parsec String u Ty
parseBoolType = reserved "Bool" >> return TyBool

parseFunctionType :: Parsec String u (Ty -> Ty -> Ty)
parseFunctionType = arrow >> return TyArr

parseType :: Parsec String u Ty
parseType = parseBoolType `chainr1` parseFunctionType
