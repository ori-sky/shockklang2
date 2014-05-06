module Tokenizer
( Token(..)
, isLambda
, tokenize
) where

import Data.Char (isSpace)

data Token = Equals
           | Lambda
           | Number Double
           | Identifier String
             deriving (Show)

isIdentifierChar :: Char -> Bool
isIdentifierChar '(' = False
isIdentifierChar ')' = False
isIdentifierChar '=' = False
isIdentifierChar '\\' = False
isIdentifierChar c = not (isSpace c)

dropComment :: String -> String
dropComment (')':')':xs) = xs
dropComment (_:xs) = xs
dropComment "" = []

isLambda :: Token -> Bool
isLambda Lambda = True
isLambda _ = False

tokenize :: String -> [Token]
tokenize ('(':'(':xs) = tokenize (dropComment xs)
tokenize ('=':xs) = Equals : tokenize xs
tokenize ('\\':xs) = Lambda : tokenize xs
tokenize "" = []
tokenize s@(c:cs) = if isSpace c
    then tokenize cs
    else if null id
        then error "tokenizer: syntax error"
        else case reads id of
            [(x, "")]  -> Number x : tokenize xs
            _         -> Identifier id : tokenize xs
  where (id, xs) = break (not.isIdentifierChar) s
