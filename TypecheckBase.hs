module TypecheckBase where

import AST
import Parser
import Control.Monad
import Data.List.Split
import BinaryMath (intToBin, uintToBin)
import Misc (slice)
import Frame

data ValOrErr a =
        Val a |
        Err String
    deriving (Show, Eq)

instance Monad ValOrErr where
    -- | A parser that always succeeds and returns 'x'
    return x  = Val x
    -- | A parser that always fails, with the given message
    fail msg  = Err msg
    -- | The "bind" (or "and-then") operator
    a >>= f =
        case a of
            (Err e) -> (Err e)

            -- if a is a value, then we want to run the function f on it
            (Val t) -> f t

instance Functor ValOrErr where
    fmap = liftM

instance Applicative ValOrErr where
    pure = return
    (<*>) = ap

instance Alternative ValOrErr where
    empty = fail "fail"
    (Err e) <|> b = b
    (Val v) <|> b = (Val v)

-- frame of variables mapped to types, code
type TypeEntry = (Type, Safety)
type TypeEnv = (Frame TypeEntry, String)

commonSupertype :: Type -> Type -> Maybe Type
commonSupertype t1 t2 =
        (commonSupertypeHalf t1 t2) <|> (commonSupertypeHalf t2 t1)
    where
        commonSupertypeHalf :: Type -> Type -> Maybe Type

        -- can't promote anything into bits type
        commonSupertypeHalf (BitsType b1) (BitsType b2) =
            if b1 == b2
                then Just (BitsType b1)
                else Nothing

        -- can't promote anything into bool type
        commonSupertypeHalf (BoolType) (BoolType) = Just BoolType

        -- UIntType promotion
        commonSupertypeHalf (UIntType b1) (UIntType b2) = Just (UIntType (max b1 b2))
        commonSupertypeHalf (UIntType b1) (IntType b2) = Just (IntType (max (b1 + 1) b2))
        commonSupertypeHalf (UIntType b1) (FixedType i2 d2) = Just (FixedType (max (b1 + 1) i2) d2)

        -- IntType promotion
        commonSupertypeHalf (IntType b1) (IntType b2) = Just (IntType (max b1 b2))
        commonSupertypeHalf (IntType b1) (FixedType i2 d2) = Just (FixedType (max b1 i2) d2)

        -- FixedType promotion
        commonSupertypeHalf (FixedType i1 d1) (FixedType i2 d2) = Just (FixedType (max i1 i2) (max d1 d2))

        -- if none of these promotions work
        commonSupertypeHalf _ _ = Nothing

{-
t1 is a subtype of t2 if the common supertype of t1 and t2 is just t2
-}
isSubtype :: Type -> Type -> Bool
isSubtype t1 t2 =
    case (commonSupertype t1 t2) of
        (Just supertype) -> supertype == t2
        -- if there's no common supertype, neither can be a subtype of the
        -- other
        Nothing  -> False


-- takes two types, and performs implicit casts so that they are in the same
-- class (eg. Fixed, UInt, ...)
alignTypes :: Type -> Type -> Maybe (Type, Type)
alignTypes t1 t2 = tryPromotet1 <|> tryPromotet2
    where
        tryPromotet1 = (promoteType t1 t2) >>= \t1' -> return (t1', t2)
        tryPromotet2 = (promoteType t2 t1) >>= \t2' -> return (t1, t2')

-- promotes t1 to the class of t2
-- the types will not be identical, but they will be in the same class
-- (eg. Int, Fixed, ...)
promoteType :: Type -> Type -> Maybe Type

-- can't promote anything into bits type
promoteType t1@(BitsType _) (BitsType _) = Just t1

-- can't promote anything into bool type
promoteType (BoolType) (BoolType) = Just BoolType

-- UIntType promotion
promoteType t1@(UIntType _) (UIntType _) = Just t1
promoteType (UIntType usize) (IntType _) = Just (IntType (usize + 1))
promoteType (UIntType usize) (FixedType _ _) = Just (FixedType (usize + 1) 0)

-- IntType promotion
promoteType t1@(IntType _) (IntType _) = Just t1
promoteType (IntType isize) (FixedType _ _) = Just (FixedType isize 0)

-- FixedType promotion
promoteType t1@(FixedType _ _) (FixedType _ _) = Just t1

-- if none of these promotions work
promoteType _ _ = Nothing


-- (start, end) -> whole input concrete syntax -> selected portion
showCode :: ParseString -> String -> String
showCode ((sline, scol), (eline, ecol)) concrete
    | sline == eline     = printLines [slice (scol - 1) (ecol - 1) (lineList!!(sline - 1))]
    | eline == sline + 1 = printLines [firstLine, lastLine]
    | otherwise          = printLines ([firstLine] ++ middleLines ++ [lastLine])

    where
        lineList = splitOn "\n" concrete

        -- these variables are only accurate if the code is distributed over
        -- multiple lines
        firstLine = drop (scol - 1) (lineList!!(sline - 1))
        lastLine = take (ecol - 1) (lineList!!(eline - 1))
        middleLines = (slice (sline - 1 + 1) (eline - 1 - 1) lineList)

        printLines :: [String] -> String
        printLines l = foldl1 combLines l
            where
                combLines a b = a ++ " / " ++ b


-- all the expressions, all the types of these expressions, the operator, the code
makeOpTypeError :: (Show a) => [PExpr] -> [Type] -> a -> TypeEnv -> String
makeOpTypeError exprs types op env =
    case (exprs, types) of
        ([e1], [t1])         -> opstr ++ (snippetMsg e1 t1 env)
        ([e1, e2], [t1, t2]) -> opstr ++ (snippetMsg e1 t1 env) ++ " and " ++ (snippetMsg e2 t2 env)
    where
        -- combine all expressions involved in this type error to get the
        -- overall parse string
        (firstExpr:restExpr) = exprs
        s = foldl (\s e -> combParseStrings s (getExtra e)) (getExtra firstExpr) restExpr

        opstr = (makeLineMessage s) ++ (show op) ++ " can't be applied to "

-- expression, expression type, expected type
makeTypeErr :: PExpr -> Type -> Type -> TypeEnv -> String
makeTypeErr expr exprType expectedType env =
        (makeLineMessage (getExtra expr)) ++ "could not implicit cast " ++ msg ++ " to " ++ (show expectedType)
    where msg = (snippetMsg expr exprType env)

makeCastError :: PExpr -> Type -> Type -> TypeEnv -> String
makeCastError expr exprType expectedType env =
        (makeLineMessage (getExtra expr)) ++ "could not cast " ++ msg ++ " to " ++ (show expectedType)
    where msg = (snippetMsg expr exprType env)

makeUndefVarErr :: ParseString -> Var -> String
makeUndefVarErr s v = (makeLineMessage s) ++ "the variable `" ++ v ++ "` was used before it was declared"

makeRedefVarErr :: ParseString -> Var -> String
makeRedefVarErr s var =
    (makeLineMessage s) ++ "cannot redefine variable `" ++ var ++ "`"

-- get a descriptive message (eg `1` (UInt1)) about a snippet of code
snippetMsg :: PExpr -> Type -> TypeEnv -> String
snippetMsg e t (_, code) = message
     where
         codeSnippet = showCode (getExtra e) code
         message = "`" ++ codeSnippet ++ "` " ++ "(" ++ (show t) ++ ")"

makeLineMessage :: ParseString -> String
makeLineMessage ((sline, scol), (eline, ecol)) =
    if sline == eline
        then ("line " ++ (show sline) ++ ": ")
        else ("lines " ++ (show sline) ++ "-" ++ (show eline) ++ ": ")
