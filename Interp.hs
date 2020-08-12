module Interp where

import AST
import

-- frame of variables mapped to types, code
type VarEntry = (Type, Safety)
type TypeEnv = (Frame VarEntry, String)


interpExpr :: Expr ->
