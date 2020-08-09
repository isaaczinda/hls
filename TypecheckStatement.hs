module TypecheckStatement where

import TypecheckExpr
import TypecheckBase
import Data.Map (empty)
import Parser (parse, block, expr)
import AST

typecheckStatement :: Statement -> TypeEnv -> (TypeEnv, [String])

typecheckStatement (Declare s varTy var expr) env@(frame, code) =
    -- try to create a new variable
    case (newVar frame var varTy) of
        Just frame' ->
            case (typecheckExpr expr env) of
                    (Err e)      -> (env, [e])
                    (Val exprTy) ->
                        -- it's okay if the expression needs to be implicit casted
                        -- to meet the type of the variable
                        case isSubtype exprTy varTy of
                            False  -> (env, [makeAssignTypeErr var varTy expr exprTy env])
                            True   -> ((frame', code), [])

        Nothing -> (env, ["cannot redefine variable `" ++ var ++ "`"])


typecheckStatement (Assign s var expr) env@(frame, code) =
    -- try to create a new variable
    case (getVar frame var) of
        -- if we are able to get the type of the variable
        Just (varTy) ->
            case (typecheckExpr expr env) of
                (Err e)      -> (env, [e])
                (Val exprTy) ->
                    -- it's okay if the expression needs to be implicit casted
                    -- to meet the type of the variable
                    case isSubtype exprTy varTy of
                        False  -> (env, [makeAssignTypeErr var varTy expr exprTy env])
                        True -> (env, [])
        -- if we aren't able to get the type of the variable
        Nothing ->
            (env, ["the variable `" ++ var ++ "` was used before it was declared"])


typecheckBlock :: Block -> TypeEnv -> (TypeEnv, [String])
typecheckBlock statements env =
        foldl comb (env, []) statements
    where
        comb :: (TypeEnv, [String]) -> Statement -> (TypeEnv, [String])
        comb (env, errs) statement =
                (env', errs ++ newErrs)
            where
                (env', newErrs) = typecheckStatement statement env


makeAssignTypeErr :: Var -> Type -> Expr -> Type -> TypeEnv -> String
makeAssignTypeErr var varTy expr exprTy env =
        "the variable `" ++ var ++ "` (" ++ (show varTy) ++ ") cannot be assigned the value of " ++ exprCode
    where
        exprCode = snippetMsg expr exprTy env

runProgram :: String -> [String]
runProgram s = errs
    where
        astBlock = parse block s
        (env, errs) = typecheckBlock astBlock ((Global empty), s)

runExpr :: String -> ValOrErr Type
runExpr s = typecheckExpr astExpr ((Global empty), s)
    where
        astExpr = parse expr s
