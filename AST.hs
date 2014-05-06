{-# LANGUAGE FlexibleInstances #-}

module AST where

data AST = Scope [AST]
         | Num Double
         | Boolean Bool
         | Identifier String
         | Var String AST
         | Lambda [String] AST [AST]
         | Binding1 (AST -> AST) [AST]
         | Binding2 (AST -> AST -> AST) [AST]
         | Binding3 (AST -> AST -> AST -> AST) [AST]
         | Undefined
           deriving (Show)

instance Show (AST -> AST) where
    show _ = "<Native Code>"
instance Show (AST -> AST -> AST) where
    show _ = "<Native Code>"
instance Show (AST -> AST -> AST -> AST) where
    show _ = "<Native Code>"
