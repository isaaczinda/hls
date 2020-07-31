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

-- typecheck bool

typecheck (Exactly _ (Bool True)) _ = Val BoolType
typecheck (Exactly _ (Bool False)) _ = Val BoolType

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
        btype <- typecheck b code

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

-- typecheck negative operator
typecheck (UnExpr s NegOp e) code =
    do
        t <- typecheck e code
        case t of
            (UIntType bits) -> Val (IntType (bits + 1))
            (IntType bits) -> Val (IntType (bits + 1))
            (FixedType intbits decbits) -> Val (FixedType (intbits + 1) decbits)
            otherwise -> Err (makeTypeError [e] [t] NegOp code)

-- typecheck list construction
typecheck (List s []) code = Val EmptyListType

typecheck (List s exprs) code = do
    firstType <- typecheck (head exprs) code
    let firstItem = Val (getParseString (head exprs), firstType)
    (_, finalType) <- foldl combTypes firstItem exprs
    Val (ListType finalType (length exprs))

        where
            combTypes :: ValOrErr (ParseString, Type) -> Expr -> ValOrErr (ParseString, Type)
            combTypes sofar e2 =
                do
                    (s1, t1) <- sofar
                    t2 <- typecheck e2 code

                    -- try to align the type of the elements in the list so
                    -- far with the type of the next element
                    case (alignTypesStrict t1 t2) of
                        Just t' ->
                            let
                                s' = combParseStrings s1 (getParseString e2)
                            in
                                Val (s', t')
                        Nothing ->
                            let
                                -- because snippetMsg takes expressions, we
                                -- have to make fake expressions in order to
                                -- use it
                                snippet1 = snippetMsg (List s1 []) t1 code -- the list
                                snippet2 = snippetMsg (List (getParseString e2) []) t2 code -- the single item
                            in
                                Err ("when constructing list, types of elements " ++ snippet1 ++ " and element " ++ snippet2 ++ " were incompatible")

-- typecheck indexing
typecheck (Index _ e i) code = do
    typecheckIndex i code
    exprType <- typecheck e code

    let snippet = snippetMsg e exprType code

    case exprType of
        (ListType t _) -> Val t
        (BitsType _)   -> Val (BitsType 1)
        otherwise      -> Err ("cannot index non-list or bits type: " ++ snippet)


-- typecheck slicing
typecheck (Slice _ e i1 i2) code = do
    typecheckImmediateIndex i1 code
    typecheckImmediateIndex i2 code
    exprType <- typecheck e code

    let snippet = snippetMsg e exprType code

    case exprType of
        t@(ListType _ _) -> Val t
        t@(BitsType _)   -> Val t
        otherwise      -> Err ("cannot slice non-list or bits type: " ++ snippet)

-- typecheck ==
typecheck e@(BinExpr _ _ EqualsOp _) code = equalityTypecheck e code

-- typecheck !=
typecheck e@(BinExpr _ _ NotEqualsOp _) code = equalityTypecheck e code

-- typecheck ||
typecheck e@(BinExpr _ _ OrOp _) code = boolBinOpTypecheck e code

-- typecheck &&
typecheck e@(BinExpr _ _ AndOp _) code = boolBinOpTypecheck e code

-- typecheck !
typecheck (UnExpr _ NotOp e) code = do
    t <- typecheck e code
    case t of
            BoolType  -> Val BoolType
            otherwise -> Err (makeTypeError [e] [t] NotOp code)

-- typecheck ~
typecheck (UnExpr _ BitNotOp e) code = do
    t <- typecheck e code
    case t of
            (BitsType n)  -> Val (BitsType n)
            otherwise -> Err (makeTypeError [e] [t] BitNotOp code)

-- typecheck ++
typecheck (BinExpr _ e1 ConcatOp e2) code = do
    t1 <- typecheck e1 code
    t2 <- typecheck e2 code

    let err = Err (makeTypeError [e1, e2] [t1, t2] ConcatOp code)

    case (t1, t2) of
        (BitsType n1, BitsType n2) -> Val (BitsType (n1 + n2))

        -- handle cases where one or more of the lists are empty
        (EmptyListType, EmptyListType) -> Val EmptyListType
        (EmptyListType, ListType t l) -> Val (ListType t l)
        (ListType t l, EmptyListType) -> Val (ListType t l)

        -- lists can only be concatenated when the elements the same type
        (ListType lt1 l1, ListType lt2 l2) ->
            if lt1 == lt2
                then Val (ListType lt1 (l1 + l2))
                else err
        otherwise -> err

-- make sure that Expr is a UInt type to be used for indexing
typecheckIndex :: Expr -> String -> ValOrErr Type
typecheckIndex e code = do
    exprType <- typecheck e code

    let snippet = snippetMsg e exprType code

    case exprType of
        t@(UIntType _) -> Val t
        otherwise -> Err ("index value " ++ snippet ++ " is not a UInt")

-- make sure that Expr type is UInt, and that its value can be computed at
-- compile time
typecheckImmediateIndex :: Expr -> String -> ValOrErr Type
typecheckImmediateIndex e code = do
    t <- typecheckIndex e code -- first make sure it's an index type

    let snippet = snippetMsg e t code

    -- now make sure that it's immediate
    case isImmdiate e of
        True  -> Val t
        False -> Err ("the value of the index " ++ snippet ++ " cannot be calculated at compile time")


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
            -- we don't need a fractional part if the integer value is the same
            -- as the whole value
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
        leadingZeroes "" = 0

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

boolBinOpTypecheck :: Expr -> String -> ValOrErr Type
boolBinOpTypecheck (BinExpr _ e1 op e2) code = do
    t1 <- typecheck e1 code
    t2 <- typecheck e2 code

    case (t1, t2) of
        (BoolType, BoolType) -> Val BoolType
        otherwise            ->
            Err (makeTypeError [e1, e2] [t1, t2] op code)

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

equalityTypecheck :: Expr -> String -> ValOrErr Type
equalityTypecheck (BinExpr _ a op b) code = do
    atype <- typecheck a code
    btype <- typecheck b code

    case alignTypesStrict atype btype of
        Nothing -> Err (makeTypeError [a, b] [atype, btype] op code)
        Just t  -> Val BoolType

parseTypecheck :: String -> (Expr, Type)
parseTypecheck code =
        case checkResult of
            (Val t) -> (ast, t)
            (Err e) -> error e
    where
        ast = parse expr code
        checkResult = typecheck ast code
