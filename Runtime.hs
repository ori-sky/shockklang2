module Runtime where

import qualified Data.Map as M
import qualified AST as AST
import Env

runASTs :: Env -> [AST.AST] -> AST.AST
runASTs env [] = result env
runASTs env (ast:asts) = runASTs (runAST env ast) asts

runLambda :: Env -> AST.AST -> AST.AST
runLambda env (AST.Lambda args ast applied) = result (runAST e ast)
  where vs = M.union (M.fromList (zip args applied)) (vars env)
        e = env {vars=vs, result=AST.Undefined}
runLambda _ _ = error "runtime: not a lambda"

runAST' :: Env -> AST.AST -> Env
runAST' env (AST.Scope asts) = env {result=runASTs env {result=AST.Undefined} asts}
runAST' env x@(AST.Num _) = env {result=x}
runAST' env x@(AST.Boolean _) = env {result=x}
runAST' env (AST.Identifier id) = env {result=r}
  where r = case M.lookup id (vars env) of
            Just ast -> ast
            Nothing  -> AST.Undefined
runAST' env (AST.Var id ast) = env {result=AST.Undefined, vars=vs}
  where vs = M.insert id (result (runAST env ast)) (vars env)
runAST' env lambda@(AST.Lambda _ _ _) = env {result=lambda}
runAST' env binding@(AST.Binding1 _ _) = env {result=binding}
runAST' env binding@(AST.Binding2 _ _) = env {result=binding}
runAST' env binding@(AST.Binding3 _ _) = env {result=binding}
runAST' _ AST.Undefined = error "runtime: Undefined"

runAST :: Env -> AST.AST -> Env
runAST env@(Env {result=AST.Lambda args astL applied}) ast = env {result=r}
  where l = AST.Lambda args astL (result (runAST env {result=AST.Undefined} ast) : applied)
        r = if length applied + 1 == length args then runLambda env l else l
runAST env@(Env {result=AST.Binding1 f applied}) ast = env {result=r}
  where res = result (runAST env {result=AST.Undefined} ast)
        r = if length applied + 1 == 1 then f res else AST.Binding1 f (res : applied)
runAST env@(Env {result=AST.Binding2 f applied}) ast = env {result=r}
  where res = result (runAST env {result=AST.Undefined} ast)
        r = if length applied + 1 == 2 then f (applied!!0) res else AST.Binding2 f (res : applied)
runAST env@(Env {result=AST.Binding3 f applied}) ast = env {result=r}
  where res = result (runAST env {result=AST.Undefined} ast)
        r = if length applied + 1 == 3 then f (applied!!1) (applied!!0) res else AST.Binding3 f (res : applied)
runAST env ast = runAST' env ast

run :: AST.AST -> String
run = show . result . runAST defaultEnv
