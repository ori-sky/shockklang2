module Tokenizer
( Token(..)
, isScopeEnd
, isLambda
, tokenize
) where

import Data.Char (isSpace)

data Token = Equals
           | ScopeBegin
           | ScopeEnd
           | Lambda
           | Number Double
           | Identifier String
             deriving (Show)

isIdentifierChar :: Char -> Bool
isIdentifierChar '(' = False
isIdentifierChar ')' = False
isIdentifierChar '\\' = False
isIdentifierChar c = not (isSpace c)

dropComment :: String -> String
dropComment ('~':')':xs) = xs
dropComment (_:xs) = dropComment xs
dropComment "" = []

isScopeEnd :: Token -> Bool
isScopeEnd ScopeEnd = True
isScopeEnd _ = False
isLambda :: Token -> Bool
isLambda Lambda = True
isLambda _ = False

tokenize :: String -> [Token]
tokenize ('(':'~':xs) = tokenize (dropComment xs)
tokenize ('(':xs) = ScopeBegin : tokenize xs
tokenize (')':xs) = ScopeEnd : tokenize xs
tokenize ('=':' ':xs) = Equals : tokenize xs
tokenize ('\\':xs) = Lambda : tokenize xs
tokenize "" = []
tokenize s@(c:cs) = if isSpace c
    then tokenize cs
    else if null id
        then error "tokenizer: syntax error"
        else case reads id of
            [(x, "")]  -> Number x : tokenize xs
            _         -> Identifier id : tokenize xs
  where (id, xs) = span isIdentifierChar s
