module Env where

import qualified Data.Map as M
import qualified AST as AST

data Env = Env
    { vars      :: M.Map String AST.AST
    , result    :: AST.AST
    }
    deriving (Show)

defaultEnv = Env
    { vars      = M.empty
    , result    = AST.Undefined
    }
