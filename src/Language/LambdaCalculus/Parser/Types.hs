module Language.LambdaCalculus.Parser.Types
  ( parseType
  ) where

import Language.LambdaCalculus.Parser.Common
import Language.LambdaCalculus.Types

import Text.Parsec

parseTyBool :: LCParser Ty
parseTyBool = reserved "Bool" >> return TyBool

parseTyArr :: LCParser (Ty -> Ty -> Ty)
parseTyArr = arrow >> return TyArr

parseType :: LCParser Ty
parseType = parseTyBool `chainr1` parseTyArr
