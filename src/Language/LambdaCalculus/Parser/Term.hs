{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Language.LambdaCalculus.Parser.Term
  ( parseLC
  ) where

import Data.List (elemIndex)

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Parser.Common
import Language.LambdaCalculus.Parser.Types

import Text.Parsec

parseAbs :: LCParser Term
parseAbs = do
  pos <- getPosition
  backslash
  v <- identifier
  modifyState (v :)
  colon
  ty <- parseType
  dot
  term <- parseTerm
  modifyState tail
  return $ TmAbs (infoFrom pos) v ty term

parseVar :: LCParser Term
parseVar = do
  v <- identifier
  list <- getState
  findVar v list

findVar :: String -> BoundContext -> LCParser Term
findVar v list = case elemIndex v list of
  Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
  Just n  -> do
    pos <- getPosition
    return $ TmVar (infoFrom pos) n (length list)

parseTrue :: LCParser Term
parseTrue = do
  pos <- getPosition
  reserved "true"
  return $ TmTrue (infoFrom pos)

parseFalse :: LCParser Term
parseFalse = do
  pos <- getPosition
  reserved "false"
  return $ TmFalse (infoFrom pos)

parseIf :: LCParser Term
parseIf = do
  pos <- getPosition
  reserved "if"
  c <- parseTerm
  reserved "then"
  t <- parseTerm
  reserved "else"
  e <- parseTerm
  return $ TmIf (infoFrom pos) c t e

parseNonApp :: LCParser Term
parseNonApp =  parens parseTerm   -- (M)
           <|> parseAbs           -- $\lambda$x.M
           <|> parseIf            -- if a then b else c
           <|> parseTrue          -- true
           <|> parseFalse         -- false
           <|> parseVar           -- x

parseTerm :: LCParser Term
parseTerm = chainl1 parseNonApp $ do
  whiteSpace
  pos <- getPosition
  return $ TmApp (infoFrom pos)

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "simply typed lambda-calculus"

parseLC :: String -> Either ParseError Term
parseLC = parseWith (whiteSpace >> parseTerm)
