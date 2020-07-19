module Typecheck where

import AST
import Parser
import ParserBase

import Data.Map (Map, lookup, empty, insert)

import Control.Applicative
import Control.Monad (ap, liftM)

import Data.List.Split

data ValOrErr a =
        Val a |
        Err String



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


-- returns that 0 can be represented in 0 bits, which is technically correct
-- but not very useful
minUnsignedBits :: Int -> Int
minUnsignedBits x
    | x < 0     = error "negative number cannot be represented using unsigned"
    | otherwise = ceiling (logBase 2 (fromIntegral (x + 1)))

-- gets the number of bits needed to hold an unsigned positive number
unsignedBits :: Int -> Int
unsignedBits x
    | x == 0    = 1
    | otherwise = minUnsignedBits x

signedBits :: Int -> Int
signedBits x
    | x >= 0    = (minUnsignedBits x) + 1
    -- x - 1 because negative numbers are allowed to be 1 larger than
    -- positive numbers because of two's complement
    | otherwise = (minUnsignedBits ((abs x) - 1)) + 1

-- TODO: make this work for large decimals
fractionBits :: Double -> Int
fractionBits x
    | x >= 1    = error "fraction was >= 1"
    | x < 0    = error "fraction was < 0"
    | otherwise = findFractionBits x 0

    where
        -- bitsUsed starts at 0 and climbs
        findFractionBits :: Double -> Int -> Int
        findFractionBits value bitsUsed =
                if value == 0 then
                    bitsUsed
                else
                    -- if we would use more than 32 fractional bits, this is
                    -- probably an infinite binary decimal and we should stop
                    -- and just use a Fixed?.32 type
                    if bitsUsed == 32 then 32
                    else findFractionBits value' (bitsUsed + 1)
            where
                -- the value that putting a 1 in the next place would add to
                -- the number
                placeValue = 1.0 / (2 ^^ (bitsUsed + 1))
                value' =
                    if value - placeValue >= 0 then
                        value - placeValue
                    else
                        value



bitsInType :: Type -> Int
bitsInType (BitsType a) = a
bitsInType (FixedType a b) = a + b
bitsInType (IntType a) = a
bitsInType (UIntType a) = a
bitsInType BoolType = 1

--
-- -- gets whether first argument is a subtype of the second argument
-- commonSupertypeHalf :: Type -> Type -> Maybe Type
--
-- {-
-- commonSupertype calls that contain BitsType:
--  * (BitsType, BitsType)
--  * (BitsType, FixedType)
--  * (BitsType, IntType)
--  * (BitsType, UIntType)
--  * (BitsType, BoolType)
-- -}
--
-- commonSupertypeHalf a (BitsType b) = Just (BitsType (max (bitsInType a) b))
--
-- {-
-- commonSupertype calls that contain BoolType:
--  * (BoolType, BoolType)
--  * (BoolType, FixedType)
--  * (BoolType, IntType)
--  * (BoolType, UIntType)
-- -}
-- commonSupertypeHalf a BoolType = Just (BitsType (max (bitsInType a) 1))
--
--
-- ---  commonSupertype calls that contain FixedType:
-- -- (FixedType, FixedType)
-- commonSupertypeHalf (FixedType aint adec) (FixedType bint bdec) =
--     Just (FixedType (max aint bint) (max adec bdec))
--
-- -- (FixedType, IntType)
-- commonSupertypeHalf (IntType a) (FixedType bint bdec) =
--     Just (FixedType (max a bint) bdec)
--
-- -- (FixedType, UIntType)
-- commonSupertypeHalf (UIntType a) (FixedType bint bdec) =
--     Just (FixedType (max (a + 1) bint) bdec)
--
--
-- ---  commonSupertype calls that contain IntType:
-- -- (IntType, IntType)
-- commonSupertypeHalf (IntType a) (IntType b) =
--     Just (IntType (max a b))
--
-- -- (IntType, UIntType)
-- -- when representing UInt as an int, we need to add 1 bit for sign
-- commonSupertypeHalf (IntType a) (UIntType b) =
--     Just (IntType (max a (b + 1)))
--
-- --- commonSupertype calls that contain UIntType:
-- commonSupertypeHalf (UIntType a) (UIntType b) =
--     Just (IntType (max a b))
--
-- commonSupertypeHalf _ _ = Nothing
--
--
-- -- like commonSupertypeHalf, but arguments can be passed a, b or b, a
-- commonSupertype :: Type -> Type -> Maybe Type
-- commonSupertype a b = (commonSupertypeHalf a b) <|> (commonSupertypeHalf b a)


-- takes two types, and performs implicit casts so that they are in the same
-- class (eg. Fixed, UInt, ...)

alignTypes :: Type -> Type -> Maybe (Type, Type)
alignTypes t1 t2 = tryPromotet1 <|> tryPromotet2
        where
            tryPromotet1 = (promoteType t1 t2) >>= \t1' -> return (t1', t2)
            tryPromotet2 = (promoteType t2 t1) >>= \t2' -> return (t1, t2')


-- promotes t1 to the class of t2
-- handles cases where t1 and t2 are already in the same class
promoteType :: Type -> Type -> Maybe Type

-- can promote anything into a bits type
promoteType t1 (BitsType _) = Just (BitsType (bitsInType t1))


-- BoolType promotion
promoteType (BoolType) (BoolType) = Just BoolType

-- UIntType promotion
promoteType t1@(UIntType _) (UIntType _) = Just t1
promoteType (UIntType usize) (IntType _) = Just (IntType (usize + 1))
promoteType (UIntType usize) (FixedType _ _) = Just (FixedType (usize + 1) 0)

-- IntType promotion
promoteType t1@(IntType _) (IntType _) = Just t1
promoteType (IntType isize) (FixedType _ _) = Just (FixedType isize 0)

-- FixedType promotion
promoteType t1@(FixedType _ _ ) (FixedType _ _) = Just t1

-- if none of these promotions work
promoteType _ _ = Nothing



slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

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

-- Expression, Type, Code
getTypeParseString :: Expr -> Type -> String -> String
getTypeParseString e t code = message
     where
         codeSnippet = showCode (getParseString e) code
         message = "`" ++ codeSnippet ++ "` " ++ (show t)

-- typecheck literals

typecheck :: Expr -> String -> ValOrErr Type

typecheck (Exactly _ (Dec a)) _
    | a >= 0    = Val (UIntType (unsignedBits a))
    | otherwise = Val (IntType (signedBits a))

typecheck (Exactly _ (Fixed a b)) _ = Val (FixedType (signedBits a) fracBits)
    where
        fracString = "0." ++ ((show b) :: String)
        fracBits = fractionBits (read fracString :: Double)


typecheck (Exactly _ (Bin a)) _ = Val (BitsType (length a))

typecheck (Exactly _ (Hex a)) _ = Val (BitsType ((length a) * 4))


-- typecheck addition

typecheck (BinExpr s a PlusOp b) code =
    do
        atype <- typecheck a code
        btype <- typecheck b code

        -- alignTypes brings the types into the same class so that HOPEFULLY
        -- we can do addition on them
        case (alignTypes atype btype) of
            Just (UIntType abits, UIntType bbits) ->
                Val (UIntType ((max abits bbits) + 1))
            Just (IntType abits, IntType bbits) ->
                Val (IntType ((max abits bbits) + 1))
            Just (FixedType aint adec, FixedType bint bdec) ->
                Val (FixedType ((max aint bint) + 1) (max adec bdec))
            otherwise ->
                let
                    parta = getTypeParseString a atype code
                    partb = getTypeParseString b btype code
                    message = "+ operator can't be applied to " ++ parta ++ " and " ++ partb
                in (Err message)

-- typecheck variables

typecheck (Variable s _) _ = error "variable declarations aren't supported yet"


parseTypecheck :: String -> (Expr, Type)
parseTypecheck code =
        case checkResult of
            (Val t) -> (ast, t)
            (Err e) -> error e
    where
        ast = parse expr code
        checkResult = typecheck ast code
