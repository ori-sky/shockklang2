module Parser (parse) where

import qualified AST as AST
import qualified Tokenizer as T

parseScope :: [T.Token] -> ([AST.AST], [T.Token])
parseScope [] = error "parser: end of stream"
parseScope (T.ScopeEnd : ts) = ([], ts)
parseScope ts = (ast : asts, tsss)
  where (ast, tss) = parseAST ts
        (asts, tsss) = parseScope tss

parseAST :: [T.Token] -> (AST.AST, [T.Token])
parseAST (T.Number x : ts) = (AST.Num x,  ts)
parseAST (T.Identifier name : ts) = (AST.Identifier name, ts)
parseAST (T.Equals : T.Identifier id : ts) = (AST.Var id ast, tss)
  where (ast, tss) = parseAST ts
parseAST (T.ScopeBegin : ts) = (AST.Scope asts, tss)
  where (asts, tss) = parseScope ts
parseAST (T.Lambda : ts) = (AST.Lambda (map (\(T.Identifier n) -> n) ids) ast [], tsss)
  where (ids, _:tss) = break T.isLambda ts
        (ast, tsss) = parseAST tss
parseAST [] = error "parser: end of stream"
parseAST _ = error "parser: syntax error"

parseAll :: [T.Token] -> [AST.AST]
parseAll [] = []
parseAll ts = ast : parseAll tss
  where (ast, tss) = parseAST ts

parse :: [T.Token] -> AST.AST
parse = AST.Scope . parseAll
