module TypecheckStatement where

import TypecheckExpr
import TypecheckBase
import Parser (parse, block, expr)
import AST
import Frame
import Control.Applicative ((<|>))

-- Slice _ []

-- checks whether or not an assignment-type to a variable of a specific type
-- is valid
castForAssign :: PExpr -> Type -> Safety -> TypeEnv -> ValOrErr TExpr
castForAssign pexpr varTy safety env =
    do
        texpr <- (typecheckExpr pexpr env)

        let
            exprTy = getExtra texpr

            -- see if we can implicit cast expr --> var
            implicitCastResult = case isSubtype exprTy varTy of
                True  -> Val (Cast varTy varTy texpr)
                False -> Err (makeTypeErr pexpr exprTy varTy env)

            -- leverage the explicit cast typechecker to see if we can explicit
            -- cast expr --> var
            explicitCastResult = case typecheckExpr (Cast (getExtra pexpr) varTy pexpr) env of
                (Val e) -> Val (Cast varTy varTy texpr)
                (Err e) -> Err (makeTypeErr pexpr exprTy varTy env)

        case safety of
            Safe -> implicitCastResult
            Unsafe -> (implicitCastResult <|> explicitCastResult)



data CheckOrErrs a =
    Check a | -- the typechecked value
    Errs [String] -- errors that have been produced while trying to typecheck

type StatementOrErrs = CheckOrErrs TStatement
type BlockOrErrs = CheckOrErrs TBlock


typecheckStatement :: PStatement -> TypeEnv -> (TypeEnv, StatementOrErrs)

typecheckStatement (Declare s safety varTy var expr) env@(frame, code) =
    -- try to create a new variable
    case (newVar frame var (varTy, safety)) of
        Just frame' ->
            -- new frame' is NOT passed to castForAssign expresison evaluator
            -- so we can't write `int i = i + 1;`
            case castForAssign expr varTy safety env of
                (Err e)     -> (env, Errs [e])
                (Val texpr) ->
                    ((frame', code), Check (Declare EmptyListType safety varTy var texpr))
        Nothing -> (env, Errs [makeRedefVarErr s var])

typecheckStatement (Assign s var expr) env@(frame, code) =
    -- try to create a new variable
    case (getVar frame var) of
        -- if we are able to get the type of the variable
        Just (varTy, safety) ->
            case castForAssign expr varTy safety env of
                (Err e)      -> (env, Errs [e])
                (Val texpr)  -> (env, Check (Assign EmptyListType var texpr))
        -- if we aren't able to get the type of the variable it
        -- doesn't exist yet
        Nothing -> (env, Errs [makeUndefVarErr s var])

typecheckStatement (For s initial check inc block) env@(frame, code) =
        (env, retVal)
    where
        -- create a new local context to store new variables in
        innerEnv = ((Local empty frame), code)

        (innerEnv', initialRet) = typecheckStatement initial innerEnv
        (initialErrs, initial') = case initialRet of
            (Errs errs) -> (errs, Nothing)
            (Check stat) -> ([], Just stat)

        (checkErrs, check') = case typecheckExpr check innerEnv' of
            (Err err) -> ([err], Nothing)
            (Val expr) ->
                case (getExtra expr) of
                    (BoolType)  -> ([], Just expr)
                    (checkType) -> ([makeTypeErr check checkType BoolType innerEnv'], Nothing)

        (incErrs, inc') = case inc of
            (Assign _ _ _) ->
                case snd (typecheckStatement inc innerEnv') of
                    Errs errs -> (errs, Nothing)
                    Check stat -> ([], Just stat)
            otherwise      -> ([(makeLineMessage s) ++ "increment clause in for statement did not assign to a variable"], Nothing)


        (innerEnv'', blockRet) = typecheckBlock block innerEnv'
        (blockErrs, block') =
            case blockRet of
                Check block -> ([], Just block)
                Errs errs -> (errs, Nothing)

        allErrs = initialErrs ++ checkErrs ++ incErrs ++ blockErrs
        finalStat = do
            a <- initial'
            b <- check'
            c <- inc'
            d <- block'
            return (For EmptyListType a b c d)
        retVal = case finalStat of
            Just v  -> Check v
            Nothing -> Errs allErrs


typecheckStatement (If s cond ifBlock maybeElseBlock) env@(frame, code) =
        (env, retVal)
    where
        (condErrs, cond') =
            case (typecheckExpr cond env) of
                Val env' ->
                    case (getExtra env') of
                        (BoolType) -> ([], Just env')
                        (condType) -> ([makeTypeErr cond condType BoolType env], Nothing)
                Err e -> ([e], Nothing)


        (ifErrs, ifBlock') =
            case snd (typecheckBlock ifBlock ((Local empty frame), code)) of
                Check v -> ([], Just v)
                Errs e -> (e, Nothing)

        (elseErrs, maybeElseBlock') =
            case maybeElseBlock of
                Just elseBlock ->
                    case snd (typecheckBlock elseBlock ((Local empty frame), code)) of
                        Check v -> ([], Just (Just v))
                        Errs e -> (e, Nothing)
                Nothing -> ([], Just Nothing)

        allErrs = condErrs ++ ifErrs ++ elseErrs
        finalStat = do
            a <- cond'
            b <- ifBlock'
            c <- maybeElseBlock'
            return (If EmptyListType a b c)
        retVal = case finalStat of
            Just v  -> Check v
            Nothing -> Errs allErrs

typecheckBlock :: PBlock -> TypeEnv -> (TypeEnv, BlockOrErrs)
typecheckBlock statements env =
        foldl comb (env, Check []) statements
    where
        comb :: (TypeEnv, BlockOrErrs) -> PStatement -> (TypeEnv, BlockOrErrs)
        comb (env, input) statement =
                case input of
                    -- if the input is a list of successfully typechecked
                    -- statements
                    Check block ->
                        case maybeStatement' of
                            Just statement' -> (env', Check (block ++ [statement']))
                            Nothing -> (env', Errs newErrs)

                    -- if the input if an error
                    Errs errs -> (env', Errs (errs ++ newErrs))

            where
                (env', statRet) = typecheckStatement statement env
                (maybeStatement', newErrs) = case statRet of
                    Check v     -> (Just v, [])
                    Errs errs -> (Nothing, errs)
