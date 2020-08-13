module TypecheckStatement where

import TypecheckExpr
import TypecheckBase
import Data.Map (empty)
import Parser (parse, block, expr)
import AST

-- checks whether or not an assignment-type to a variable if a specific type
-- is valid
assignmentValid :: PExpr -> Type -> Type -> Safety -> Bool
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

data ValOrErrs a =
    Val a |
    Errs [String]


data Checker a = TypecheckFunction ((a ParseString) -> TypeEnv -> (TypeEnv, Maybe (a Type), [String]))



t :: Checker Statement


typecheckStatement :: PStatement -> TypeEnv -> (TypeEnv, StatementOrErrs)
typecheckStatement (Declare s safety varTy var expr) env@(frame, code) =
    -- try to create a new variable
    case (newVar frame var (varTy, safety)) of
        Just frame' -> do
            case (typecheckExpr expr env) of
                (Err e)      -> (env, Errs [e])
                (Val expr') ->
                    let exprTy = getExtra expr'
                    in case assignmentValid expr exprTy varTy safety of
                        -- if the assignment is valid, use the new frame
                        True ->  ((frame', code), Val (Declare EmptyListType safety varTy var expr'))
                        False -> (env, Errs [makeTypeErr expr exprTy varTy env])
        Nothing -> (env, Errs [makeRedefVarErr s var])


typecheckStatement (Assign s var expr) env@(frame, code) =
    -- try to create a new variable
    case (getVar frame var) of
        -- if we are able to get the type of the variable
        Just (varTy, safety) ->
            case (typecheckExpr expr env) of
                (Err e)      -> (env, Errs [e])
                (Val expr') ->
                    let exprTy = getExtra expr'
                    in case assignmentValid expr exprTy varTy safety of
                        True  -> (env, Val (Assign EmptyListType var expr'))
                        False -> (env, Errs [makeTypeErr expr exprTy varTy env])
        -- if we aren't able to get the type of the variable it
        -- doesn't exist yet
        Nothing -> (env, Errs [makeUndefVarErr s var])


typecheckStatement (For s initial check inc block) env@(frame, code) =
        (env, allErrs)
    where
        -- create a new local context to store new variables in
        innerEnv = ((Local empty frame), code)

        (innerEnv', initialRet) = typecheckStatement initial innerEnv
        (initialErrs, initial') = case initialRet of
            (Errs errs) -> (errs, Nothing)
            (Val stat) -> ([], Just stat)

        (_, checkRet) = (typecheckExpr check innerEnv')
        (checkErrs, check') = case checkRet of
            (Errs errs) -> (errs, Nothing)
            (Val stat) ->
                case (getExtra stat) of
                    (BoolType)  -> ([], Just stat)
                    (checkType) -> ([makeTypeErr check checkType BoolType innerEnv'], Nothing)

        (incErrs, inc') = case inc of
            (Assign _ _ _) ->
                case snd (typecheckStatement inc innerEnv') of
                    Errs errs -> (errs, Nothing)
                    Val stat -> ([], Just stat)
            otherwise      -> ([(makeLineMessage s) ++ "increment clause in for statement did not assign to a variable"], Nothing)


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

typecheckBlock :: PBlock -> TypeEnv -> (TypeEnv, BlockOrErrs)
typecheckBlock statements env =
        foldl comb (env, []) statements
    where
        comb :: (TypeEnv, BlockOrErrs) -> PStatement -> (TypeEnv, BlockOrErrs)
        comb (env, errs) statement =
                (env', errs ++ newErrs)
            where
                (env', statRet) = typecheckStatement statement env
                case statRet of
                    Val v -> ()
                    Errs errs ->
