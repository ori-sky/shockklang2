{-# LANGUAGE FlexibleInstances #-}

module AST where

data AST = Scope [AST]
         | Num Double
         | Identifier String
         | Var String AST
         | Lambda [String] AST [AST]
         | Binding1 (AST -> AST) [AST]
         | Binding2 (AST -> AST -> AST) [AST]
         | Undefined
           deriving (Show)

instance Show (AST -> AST) where
    show _ = "[Binding1]"
instance Show (AST -> AST -> AST) where
    show _ = "[Binding2]"
