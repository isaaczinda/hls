module TypecheckStatement where

import TypecheckExpr
import TypecheckBase
import Data.Map (empty)
import Parser (parse, block, expr)
import AST

assignmentValid :: Expr -> Type -> Type -> Safety -> Bool
assignmentValid expr exprTy varTy safety =
    case safety of
        {- it's okay if the expression needs to be
        implicit casted to meet the type of the
        variable -}
        Safe -> isSubtype exprTy varTy

        {- if the variable has been declared unsafe, we just need to make sure
        that the type of the expression can be converted into the same class
        as the type of the variable.
        -}
        Unsafe ->
            case promoteType exprTy varTy of
                Just t  -> True
                Nothing -> False

typecheckStatement :: Statement -> TypeEnv -> (TypeEnv, [String])

typecheckStatement (Declare s safety varTy var expr) env@(frame, code) =
    -- try to create a new variable
    case (newVar frame var (varTy, safety)) of
        Just frame' ->
            case (typecheckExpr expr env) of
                    (Err e)      -> (env, [e])
                    (Val exprTy) ->
                        case assignmentValid expr exprTy varTy safety of
                            -- if the assignment is valid, use the new frame
                            True ->  ((frame', code), [])
                            False -> (env, [makeTypeErr expr exprTy varTy env])
        Nothing -> (env, ["cannot redefine variable `" ++ var ++ "`"])


typecheckStatement (Assign s var expr) env@(frame, code) =
    -- try to create a new variable
    case (getVar frame var) of
        -- if we are able to get the type of the variable
        Just (varTy, safety) ->
            case (typecheckExpr expr env) of
                (Err e)      -> (env, [e])
                (Val exprTy) ->
                    case assignmentValid expr exprTy varTy safety of
                        True ->  (env, [])
                        False -> (env, [makeTypeErr expr exprTy varTy env])
        -- if we aren't able to get the type of the variable
        Nothing -> (env, [makeVarErr var])

typecheckStatement (For s initial check inc block) env@(frame, code) =
        (env, allErrs)
    where
        -- create a new local context to store new variables in
        innerEnv = ((Local empty frame), code)

        (innerEnv', initialErrs) = typecheckStatement initial innerEnv
        checkErrs =
            case (typecheckExpr check innerEnv') of
                (Val BoolType)  -> []
                (Val checkType) -> [makeTypeErr check checkType BoolType innerEnv']
                Err e           -> [e]

        incErrs = case inc of
            (Assign _ _ _) ->
                let (_, errs) = (typecheckStatement inc innerEnv')
                in errs
            otherwise      -> ["increment clause in for statement did not assign to a variable"]


        (innerEnv'', blockErrs) = typecheckBlock block innerEnv'
        allErrs = initialErrs ++ checkErrs ++ incErrs ++ blockErrs

typecheckStatement (If s cond ifBlock elseBlock) env@(frame, code) =
        (env, allErrs)
    where
        condErrs =
            case (typecheckExpr cond env) of
                (Val BoolType) -> []
                (Val condType) -> [makeTypeErr cond condType BoolType env]
                Err e          -> [e]

        (_, ifErrs) = typecheckBlock ifBlock ((Local empty frame), code)
        elseErrs = case elseBlock of
            Just block ->
                snd (typecheckBlock block ((Local empty frame), code))
            Nothing    -> []

        allErrs = condErrs ++ ifErrs ++ elseErrs

typecheckBlock :: Block -> TypeEnv -> (TypeEnv, [String])
typecheckBlock statements env =
        foldl comb (env, []) statements
    where
        comb :: (TypeEnv, [String]) -> Statement -> (TypeEnv, [String])
        comb (env, errs) statement =
                (env', errs ++ newErrs)
            where
                (env', newErrs) = typecheckStatement statement env
