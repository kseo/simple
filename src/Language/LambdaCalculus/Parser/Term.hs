module Language.LambdaCalculus.Parser.Term
  ( parseLC
  ) where

import Data.List (elemIndex)

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Parser.Common
import Language.LambdaCalculus.Parser.Types

import Text.Parsec

type BoundContext = [String]
type LCParser = Parsec String BoundContext Term

parseAbs :: LCParser -> LCParser
parseAbs termParser = do
  pos <- getPosition
  _ <- backslash
  v <- identifier
  modifyState (v :)
  _ <- colon
  ty <- parseType
  _ <- dot
  term <- termParser
  modifyState tail
  return $ TmAbs (infoFrom pos) v ty term

parseVar :: LCParser
parseVar = do
  v <- identifier
  list <- getState
  findVar v list

findVar :: String -> BoundContext -> LCParser
findVar v list = case elemIndex v list of
  Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
  Just n  -> do
    pos <- getPosition
    return $ TmVar (infoFrom pos) n (length list)

parseTrue :: LCParser
parseTrue = do
  pos <- getPosition
  reserved "true"
  return $ TmTrue (infoFrom pos)

parseFalse :: LCParser
parseFalse = do
  pos <- getPosition
  reserved "false"
  return $ TmFalse (infoFrom pos)

parseIf :: LCParser -> LCParser
parseIf termParser = do
  pos <- getPosition
  reserved "if"
  c <- termParser
  reserved "then"
  t <- termParser
  reserved "else"
  e <- termParser
  return $ TmIf (infoFrom pos) c t e

parseNonApp :: LCParser
parseNonApp =  parens parseTerm   -- (M)
           <|> parseAbs parseTerm -- $\lambda$x.M
           <|> parseIf parseTerm  -- if a then b else c
           <|> parseTrue          -- true
           <|> parseFalse         -- false
           <|> parseVar           -- x

parseTerm :: LCParser
parseTerm = chainl1 parseNonApp $ do
  whiteSpace
  pos <- getPosition
  return $ TmApp (infoFrom pos)

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "simply typed lambda-calculus"

parseLC :: String -> Either ParseError Term
parseLC = parseWith (whiteSpace >> parseTerm)
