{-
(( comment ))
= add \ x y \ + x y
add 5 6
-}

import qualified AST as AST
import qualified Tokenizer as T
import qualified Parser as P
import qualified Runtime as R

defaultScope :: AST.AST -> AST.AST
defaultScope main = AST.Scope $
      (AST.Var "++" (AST.Binding1 fInc []))
    : [main]
  where fInc (AST.Num x) = AST.Num (x + 1)
        fInc _ = error "runtime: type error"

eval :: String -> String
eval = R.run . defaultScope . P.parse . T.tokenize

main :: IO ()
main = interact (show . eval) >> putStrLn ""
