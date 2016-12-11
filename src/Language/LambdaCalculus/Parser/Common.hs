{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.LambdaCalculus.Parser.Common
  ( infoFrom
  , parens
  , identifier
  , reserved
  , reservedOp
  , whiteSpace
  , arrow
  , backslash
  , colon
  , dot
  ) where

import Language.LambdaCalculus.AST

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

lcDef :: LanguageDef st
lcDef = emptyDef
  { P.identStart      = letter
  , P.identLetter     = letter <|> char '\''
  , P.reservedNames   = ["true", "false", "if", "then", "else"]
  , P.reservedOpNames = ["->", ":", ".", "\\"]
  }

lexer       = P.makeTokenParser lcDef
parens      = P.parens lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
whiteSpace  = P.whiteSpace lexer
backslash   = P.symbol lexer "\\"
arrow       = P.symbol lexer "->"
dot         = P.dot lexer
colon       = P.colon lexer
