module AST where

data AST = Scope [AST]
         | Num Double
         | Identifier String
         | Var String AST
         | Lambda [String] AST
         | Undefined
           deriving (Show)
