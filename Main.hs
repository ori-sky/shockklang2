{-
(( comment ))
= add \ x y \ + x y
add 5 6
-}

import qualified Data.Map as M
import qualified AST as AST
import qualified Tokenizer as T
import qualified Parser as P

data Env = Env
    { vars      :: M.Map String AST.AST
    , result    :: AST.AST
    }
    deriving (Show)

defaultEnv = Env
    { vars      = M.empty
    , result    = AST.Undefined
    }

runASTs :: Env -> [AST.AST] -> AST.AST
runASTs env [] = result env
runASTs env (ast:asts) = runASTs (runAST env ast) asts

runLambda :: Env -> AST.AST -> AST.AST
runLambda env (AST.Lambda args ast applied) = result (runAST e ast)
  where vs = M.union (M.fromList (zip args applied)) (vars env)
        e = env {vars=vs, result=AST.Undefined}
runLambda _ _ = error "runtime: not a lambda"

runAST' :: Env -> AST.AST -> Env
runAST' env (AST.Scope asts) = env {result=runASTs env asts}
runAST' env (AST.Num x) = env {result=AST.Num x}
runAST' env (AST.Identifier id) = env {result=r}
  where r = case M.lookup id (vars env) of
            Just ast -> ast
            Nothing  -> AST.Undefined
runAST' env (AST.Var id ast) = env {result=AST.Undefined, vars=vs}
  where vs = M.insert id (result (runAST env ast)) (vars env)
runAST' env lambda@(AST.Lambda _ _ _) = env {result=lambda}
runAST' _ AST.Undefined = error "runtime: Undefined"

runAST :: Env -> AST.AST -> Env
runAST env@(Env {result=AST.Lambda args astL applied}) ast = env {result=r}
  where l = AST.Lambda args astL (result (runAST' env ast) : applied)
        r = if length applied + 1 == length args
            then runLambda env l
            else l
runAST env ast = runAST' env ast

run :: AST.AST -> String
run = show . runAST defaultEnv

eval :: String -> String
eval = run . P.parse . T.tokenize

main :: IO ()
main = interact (show . eval) >> putStrLn ""
