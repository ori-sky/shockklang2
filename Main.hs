{-
(( comment ))
= add \ x y \ + x y
add 5 6
-}

import qualified AST as AST
import qualified Tokenizer as T
import qualified Parser as P
import qualified Runtime as R

typeError = error "runtime: type error"

defaultScope :: AST.AST -> AST.AST
defaultScope main = AST.Scope $
      (AST.Var "true"   (AST.Boolean True))
    : (AST.Var "false"  (AST.Boolean False))
    : (AST.Var "=="     (AST.Binding2 fEq []))
    : (AST.Var "?"      (AST.Binding3 fCond []))
    : (AST.Var "++"     (AST.Binding1 fInc []))
    : (AST.Var "+"      (AST.Binding2 fAdd []))
    : [main]
  where fEq (AST.Num x) (AST.Num y) = AST.Boolean (x == y)
        fEq _ _ = typeError
        fCond (AST.Boolean cond) left right = if cond then left else right
        fCond _ _  _ = typeError
        fInc (AST.Num x) = AST.Num (x + 1)
        fInc _ = typeError
        fAdd (AST.Num x) (AST.Num y) = AST.Num (x + y)
        fAdd _ _ = typeError

eval :: String -> String
eval = R.run . defaultScope . P.parse . T.tokenize

main :: IO ()
main = interact (show . eval) >> putStrLn ""
