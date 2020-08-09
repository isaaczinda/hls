module TypecheckBase where

import AST
import Parser

import Data.Map (Map, lookup, empty, insert, member)
import Control.Applicative
import Control.Monad (ap, liftM)
import Data.List.Split

data ValOrErr a =
        Val a |
        Err String
    deriving (Show, Eq)


data Frame a =
    Local (Map String a) (Frame a) |
    Global (Map String a)

-- creates a new variable in the outermost frame
newVar :: Frame a -> String -> a -> Maybe (Frame a)
newVar frame name value =
    case frame of
        (Local m rest) ->
            if (member name m)
                then Nothing
                else Just (Local (insert name value m) rest)
        (Global m)     ->
            if (member name m)
                then Nothing
                else Just (Global (insert name value m))

getVar :: Frame a -> String -> Maybe a
getVar frame name =
    case frame of
        (Local m frame') ->
            case Data.Map.lookup name m of
                Just val -> Just val
                Nothing  -> getVar frame' name
        (Global m)       -> Data.Map.lookup name m


-- frame of variables mapped to types, code
type TypeEnv = (Frame Type, String)

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

-- automatically implement Functor and Applicative using the monad
-- definitions
instance Functor ValOrErr where
    fmap = liftM

instance Applicative ValOrErr where
    pure = return
    (<*>) = ap

class Typecheckable a where
    typecheck :: a -> TypeEnv -> ValOrErr (TypeEnv, Type)

-- takes two types and performs implicit casts so that they are identical
alignTypesStrict :: Type -> Type -> Maybe Type
alignTypesStrict t1 t2 =
    case (alignTypes t1 t2) of
        Nothing -> Nothing
        Just (BoolType, BoolType) ->
            Just BoolType
        Just (BitsType b1, BitsType b2) ->
            if b1 == b2 then Just (BitsType b1) else Nothing
        Just (IntType i1, IntType i2) ->
            Just (IntType (max i1 i2))
        Just (UIntType i1, UIntType i2) ->
            Just (UIntType (max i1 i2))
        Just ((FixedType i1 d1), (FixedType i2 d2))  ->
            Just (FixedType (max i1 i2) (max d1 d2))


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

        slice :: Int -> Int -> [a] -> [a]
        slice from to xs = take (to - from + 1) (drop from xs)


-- all the expressions, all the types of these expressions, the operator, the code
makeTypeError :: (Show a) => [Expr] -> [Type] -> a -> TypeEnv -> String
makeTypeError exprs types op env =
    case (exprs, types) of
        ([e1], [t1])         -> opstr ++ (snippetMsg e1 t1 env)
        ([e1, e2], [t1, t2]) -> opstr ++ (snippetMsg e1 t1 env) ++ " and " ++ (snippetMsg e2 t2 env)
    where
        opstr = (show op) ++ " can't be applied to "

-- get a descriptive message (eg `1` (UInt1)) about a snippet of code
snippetMsg :: Expr -> Type -> TypeEnv -> String
snippetMsg e t (_, code) = message
     where
         codeSnippet = showCode (getParseString e) code
         message = "`" ++ codeSnippet ++ "` " ++ "(" ++ (show t) ++ ")"