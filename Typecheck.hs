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


-- Converts a
fracToBin :: Double -> Double -> String
fracToBin frac maxError
    | frac < 0 || frac >= 1 = error "can only represent fraction in range [0, 1)"
    | otherwise             = fracToBinHelper frac 0
    where
    -- bitsUsed starts at 0 and climbs
    fracToBinHelper :: Double -> Int -> String
    fracToBinHelper value bitsUsed =
            if value <= maxError then ""
            else bitStr ++ (fracToBinHelper value' (bitsUsed + 1))
        where
            -- the value that putting a 1 in the next place would add to
            -- the number
            placeValue = 1.0 / (2 ^^ (bitsUsed + 1))
            bitUsed = (value - placeValue) >= 0
            bitStr = if bitUsed then "1" else "0"
            value' = if bitUsed then value - placeValue else value

bitsInType :: Type -> Int
bitsInType (BitsType a) = a
bitsInType (FixedType a b) = a + b
bitsInType (IntType a) = a
bitsInType (UIntType a) = a
bitsInType BoolType = 1

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


-- all the expressions, all the types of these expressions, the operator, the code
makeTypeError :: (Show a) => [Expr] -> [Type] -> a -> String -> String
makeTypeError exprs types op code =
    case (exprs, types) of
        ([e1], [t1])         -> opstr ++ (snippetMsg e1 t1 code)
        ([e1, e2], [t1, t2]) -> opstr ++ (snippetMsg e1 t1 code) ++ " and " ++ (snippetMsg e2 t2 code)
    where
        opstr = (show op) ++ " can't be applied to "

-- get a descriptive message (eg `1` (UInt1)) about a snippet of code
snippetMsg :: Expr -> Type -> String -> String
snippetMsg e t code = message
     where
         codeSnippet = showCode (getParseString e) code
         message = "`" ++ codeSnippet ++ "` " ++ "(" ++ (show t) ++ ")"

-- typecheck literals

typecheck :: Expr -> String -> ValOrErr Type

typecheck (Exactly _ (Dec a)) _
    | a >= 0    = Val (UIntType (unsignedBits a))
    | otherwise = Val (IntType (signedBits a))




-- typecheck fixed-point numbers
typecheck (Exactly _ (Fixed str)) _ = typecheckFixed str

-- negative fixed-point numbers
typecheck (UnExpr _ NegOp (Exactly _ (Fixed str))) _ =
    typecheckFixed ("-" ++ str)

typecheck (Exactly _ (Bin a)) _ = Val (BitsType (length a))
typecheck (Exactly _ (Hex a)) _ = Val (BitsType ((length a) * 4))


-- typecheck addition and subtraction
typecheck e@(BinExpr _ _ PlusOp _) code = addSubTypecheck e code
typecheck e@(BinExpr _ _ MinusOp _) code = addSubTypecheck e code

-- typecheck bitwise operations
typecheck e@(BinExpr _ _ BitAndOp _) code = bitOpTypecheck e code
typecheck e@(BinExpr _ _ BitOrOp _) code = bitOpTypecheck e code
typecheck e@(BinExpr _ _ BitXOrOp _) code = bitOpTypecheck e code


-- typecheck multiplication
typecheck (BinExpr _ a TimesOp b) code =
    do
        atype <- typecheck a code
        btype <- typecheck b code

        -- alignTypes brings the types into the same class so that HOPEFULLY
        -- we can do addition on them
        case (alignTypes atype btype) of
            -- UIntX * UIntY = UInt(X+Y)
            Just (UIntType abits, UIntType bbits) ->
                Val (UIntType (abits + bbits))

            -- IntX * IntY = Int(X+Y)
            -- (not (X+Y-1) since -8 (Int4) * -8 (Int4) != (Int7) which can
            -- represent 63 at most
            Just (IntType abits, IntType bbits) ->
                Val (IntType (abits + bbits))

            Just (FixedType aint adec, FixedType bint bdec) ->
                Val (FixedType (aint + bint) (adec + bdec))

            otherwise -> Err (makeTypeError [a, b] [atype, btype] TimesOp code)

-- typechecks division
typecheck (BinExpr _ a DivOp b) code =
    do
        atype <- typecheck a code
        btype <- typecheck a code

        case (alignTypes atype btype) of
            Just (UIntType abits, UIntType bbits) -> Val (UIntType abits)
            Just (IntType abits, IntType bbits) -> Val (IntType abits)
            Just (FixedType aint afrac, FixedType bint bfrac) ->
                Val (FixedType (aint + bfrac) afrac)
            otherwise -> Err (makeTypeError [a, b] [atype, btype] DivOp code)


-- typecheck variables
typecheck (Variable s _) _ = (Err "variable declarations aren't supported yet")

-- typecheck explicit casting
-- an explicit cast modified the type but MUST preserve the underlying number
-- of bits

typecheck (Cast s t' e) code =
    do
        t <- typecheck e code

        if (bitsInType t') == (bitsInType t)
            then Val (t')
            else
                Err ("cannot cast " ++ (snippetMsg e t code) ++
                " to " ++ (show t') ++
                " because they do not contain the same number of bits.")


typecheck (UnExpr s NegOp e) code =
    do
        t <- typecheck e code
        case t of
            (UIntType bits) -> Val (IntType (bits + 1))
            (IntType bits) -> Val (IntType (bits + 1))
            (FixedType intbits decbits) -> Val (FixedType (intbits + 1) decbits)
            otherwise -> Err (makeTypeError [e] [t] NegOp code)

-- input string may be any fixed point string (eg. "-12.3, 0.23, ...")
typecheckFixed :: String -> ValOrErr Type
typecheckFixed str = Val (FixedType intBits fracBits)
    where

        fracStr = last (splitOn "." str)

        wholePart = (read str) :: Double -- value of the fixed point literal
        fracRawPart = read ("0." ++ fracStr) :: Double -- value of the numbers including and after the decimal point

        intPart :: Int
        intPart = floor wholePart

        fracPart
            -- if we don't need a fractional part
            | (fromIntegral intPart) == wholePart = 0
            -- if we need to flip the fractional part as-is
            | wholePart < 0                    = 1-fracRawPart
            | otherwise                        = fracRawPart

        -- given the trailing zeroes, calculate the minimum error we can have
        -- if the number if .012, the max error is .0005
        maxError = 1 / (10^(length fracStr) * 2)

        -- representation of the fractional part in binary
        fracBin = fracToBin fracPart maxError

        -- bits in integer part (+ 1) includes the sign as well
        intBits = if intPart == 0
            then -(leadingZeroes fracBin) + 1
            else (signedBits intPart)

        fracBits = length fracBin -- bits in fractional part

        leadingZeroes :: String -> Int
        leadingZeroes (h:rest)
             | h == '0'  = 1 + (leadingZeroes rest)
             | otherwise = 0

addSubTypecheck :: Expr -> String -> ValOrErr Type
addSubTypecheck (BinExpr s a op b) code  =
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
            otherwise -> Err (makeTypeError [a, b] [atype, btype] DivOp code)



bitOpTypecheck :: Expr -> String -> ValOrErr Type
bitOpTypecheck (BinExpr _ a op b) code =
    do
        atype <- typecheck a code
        btype <- typecheck b code

        case (atype, btype) of
                (BitsType abits, BitsType bbits) ->
                    if abits == bbits
                        then Val (BitsType abits)
                        else
                            Err ((makeTypeError [a, b] [atype, btype] DivOp code)
                            ++ "because they do not have the same number of bits.")
                otherwise ->
                    Err ((makeTypeError [a, b] [atype, btype] DivOp code)
                    ++ "because they aren't both Bits")


parseTypecheck :: String -> (Expr, Type)
parseTypecheck code =
        case checkResult of
            (Val t) -> (ast, t)
            (Err e) -> error e
    where
        ast = parse expr code
        checkResult = typecheck ast code
