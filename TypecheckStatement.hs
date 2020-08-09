module TypecheckStatement where

import TypecheckBase

typecheckStatement :: Statement -> TypeEnv -> ValOrErr (TypeEnv, Type)
typecheckStatement (Declare name ty var expr) env@(frame, code) =
    -- try to create a new variable
    case (newVar env name ty) of
        Just env' -> Val ty
        Nothing   ->
