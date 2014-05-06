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
    , result    = AST.Num 0
    }

runASTs :: Env -> [AST.AST] -> AST.AST
runASTs env [] = result env
runASTs env (ast:asts) = runASTs (runAST env ast) asts

runAST :: Env -> AST.AST -> Env
runAST env (AST.Scope asts) = env {result=runASTs env asts}
runAST env (AST.Num x) = env {result=AST.Num x}
runAST env (AST.Identifier id) = env {result=r}
  where r = case M.lookup id (vars env) of
            Just ast -> ast
            Nothing  -> error "runtime: Undefined"
runAST env (AST.Var id ast) = env {result=r, vars=M.insert id r (vars env)}
  where r = result (runAST env ast)
runAST _ AST.Undefined = error "runtime: Undefined"

run :: AST.AST -> String
run = show . runAST defaultEnv

eval :: String -> String
eval = run . P.parse . T.tokenize

main :: IO ()
main = interact (show . eval) >> putStrLn ""
