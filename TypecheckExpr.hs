module TypecheckExpr (typecheck) where

import AST
import TypecheckBase
import Data.List.Split

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


instance Typecheckable Expr where
    typecheck e env = do
        t <- typecheckExpr e env
        return (env, t)

{-
Since expressions don't make any changes to the type environment, we don't need
to have the output of typecheckExpr contain a type environment.
-}
typecheckExpr :: Expr -> TypeEnv -> ValOrErr Type


-- typecheck literals
typecheckExpr (Exactly _ (Dec a)) _
    | a >= 0    = Val (UIntType (unsignedBits a))
    | otherwise = Val (IntType (signedBits a))

-- typecheck fixed-point numbers
typecheckExpr (Exactly _ (Fixed str)) _ = typecheckFixed str

-- negative fixed-point numbers
typecheckExpr (UnExpr _ NegOp (Exactly _ (Fixed str))) _ =
    typecheckFixed ("-" ++ str)

typecheckExpr (Exactly _ (Bin a)) _ = Val (BitsType (length a))
typecheckExpr (Exactly _ (Hex a)) _ = Val (BitsType ((length a) * 4))

-- typecheck bool

typecheckExpr (Exactly _ (Bool True)) _ = Val BoolType
typecheckExpr (Exactly _ (Bool False)) _ = Val BoolType

-- typecheck addition and subtraction
typecheckExpr e@(BinExpr _ _ PlusOp _) env = addSubTypecheck e env
typecheckExpr e@(BinExpr _ _ MinusOp _) env = addSubTypecheck e env

-- typecheck bitwise operations
typecheckExpr e@(BinExpr _ _ BitAndOp _) env = bitOpTypecheck e env
typecheckExpr e@(BinExpr _ _ BitOrOp _) env = bitOpTypecheck e env
typecheckExpr e@(BinExpr _ _ BitXOrOp _) env = bitOpTypecheck e env


-- typecheck multiplication
typecheckExpr (BinExpr _ a TimesOp b) env =
    do
        atype <- typecheckExpr a env
        btype <- typecheckExpr b env

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

            otherwise -> Err (makeTypeError [a, b] [atype, btype] TimesOp env)

-- typechecks division
typecheckExpr (BinExpr _ a DivOp b) env =
    do
        atype <- typecheckExpr a env
        btype <- typecheckExpr b env

        case (alignTypes atype btype) of
            Just (UIntType abits, UIntType bbits) -> Val (UIntType abits)
            Just (IntType abits, IntType bbits) -> Val (IntType abits)
            Just (FixedType aint afrac, FixedType bint bfrac) ->
                Val (FixedType (aint + bfrac) afrac)
            otherwise -> Err (makeTypeError [a, b] [atype, btype] DivOp env)


-- typecheck variables
typecheckExpr (Variable s _) _ = (Err "variable declarations aren't supported yet")


-- typecheck explicit casting
-- an explicit cast modified the type but MUST preserve the underlying number
-- of bits
typecheckExpr (Cast s t' e) env =
    do
        t <- typecheckExpr e env

        if (bitsInType t') == (bitsInType t)
            then Val (t')
            else
                Err ("cannot cast " ++ (snippetMsg e t env) ++
                " to " ++ (show t') ++
                " because they do not contain the same number of bits.")

-- typecheck negative operator
typecheckExpr (UnExpr s NegOp e) env =
    do
        t <- typecheckExpr e env
        case t of
            (UIntType bits) -> Val (IntType (bits + 1))
            (IntType bits) -> Val (IntType (bits + 1))
            (FixedType intbits decbits) -> Val (FixedType (intbits + 1) decbits)
            otherwise -> Err (makeTypeError [e] [t] NegOp env)

-- typecheck list construction
typecheckExpr (List s []) _ = Val EmptyListType

typecheckExpr (List s exprs) env = do
    firstType <- typecheckExpr (head exprs) env
    let firstItem = Val (getParseString (head exprs), firstType)
    (_, finalType) <- foldl combTypes firstItem exprs
    Val (ListType finalType (length exprs))

        where
            combTypes :: ValOrErr (ParseString, Type) -> Expr -> ValOrErr (ParseString, Type)
            combTypes sofar e2 =
                do
                    (s1, t1) <- sofar
                    t2 <- typecheckExpr e2 env

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
                                snippet1 = snippetMsg (List s1 []) t1 env -- the list
                                snippet2 = snippetMsg (List (getParseString e2) []) t2 env -- the single item
                            in
                                Err ("when constructing list, types of elements " ++ snippet1 ++ " and element " ++ snippet2 ++ " were incompatible")

-- typecheck indexing
typecheckExpr (Index _ e i) env = do
    typecheckIndex i env
    exprType <- typecheckExpr e env

    let snippet = snippetMsg e exprType env

    case exprType of
        (ListType t _) -> Val t
        (BitsType _)   -> Val (BitsType 1)
        otherwise      -> Err ("cannot index non-list or bits type: " ++ snippet)


-- typecheck slicing
typecheckExpr (Slice _ e i1 i2) env = do
    typecheckImmediateIndex i1 env
    typecheckImmediateIndex i2 env
    exprType <- typecheckExpr e env

    let snippet = snippetMsg e exprType env

    case exprType of
        t@(ListType _ _) -> Val t
        t@(BitsType _)   -> Val t
        otherwise      -> Err ("cannot slice non-list or bits type: " ++ snippet)

-- typecheck ==
typecheckExpr e@(BinExpr _ _ EqualsOp _) env = equalityTypecheck e env

-- typecheck !=
typecheckExpr e@(BinExpr _ _ NotEqualsOp _) env = equalityTypecheck e env

-- typecheck ||
typecheckExpr e@(BinExpr _ _ OrOp _) env = boolBinOpTypecheck e env

-- typecheck &&
typecheckExpr e@(BinExpr _ _ AndOp _) env = boolBinOpTypecheck e env

-- typecheck !
typecheckExpr (UnExpr _ NotOp e) env = do
    t <- typecheckExpr e env
    case t of
            BoolType  -> Val BoolType
            otherwise -> Err (makeTypeError [e] [t] NotOp env)

-- typecheck ~
typecheckExpr (UnExpr _ BitNotOp e) env = do
    t <- typecheckExpr e env
    case t of
            (BitsType n)  -> Val (BitsType n)
            otherwise -> Err (makeTypeError [e] [t] BitNotOp env)

-- typecheck ++
typecheckExpr (BinExpr _ e1 ConcatOp e2) env = do
    t1 <- typecheckExpr e1 env
    t2 <- typecheckExpr e2 env

    let err = Err (makeTypeError [e1, e2] [t1, t2] ConcatOp env)

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
typecheckIndex :: Expr -> TypeEnv -> ValOrErr Type
typecheckIndex e env = do
    exprType <- typecheckExpr e env

    let snippet = snippetMsg e exprType env

    case exprType of
        t@(UIntType _) -> Val t
        otherwise -> Err ("index value " ++ snippet ++ " is not a UInt")

-- make sure that Expr type is UInt, and that its value can be computed at
-- compile time
typecheckImmediateIndex :: Expr -> TypeEnv -> ValOrErr Type
typecheckImmediateIndex e env = do
    t <- typecheckIndex e env -- first make sure it's an index type

    let snippet = snippetMsg e t env

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

addSubTypecheck :: Expr -> TypeEnv -> ValOrErr Type
addSubTypecheck (BinExpr s a op b) env  =
    do
        atype <- typecheckExpr a env
        btype <- typecheckExpr b env

        -- alignTypes brings the types into the same class so that HOPEFULLY
        -- we can do addition on them
        case (alignTypes atype btype) of
            Just (UIntType abits, UIntType bbits) ->
                Val (UIntType ((max abits bbits) + 1))
            Just (IntType abits, IntType bbits) ->
                Val (IntType ((max abits bbits) + 1))
            Just (FixedType aint adec, FixedType bint bdec) ->
                Val (FixedType ((max aint bint) + 1) (max adec bdec))
            otherwise -> Err (makeTypeError [a, b] [atype, btype] DivOp env)

boolBinOpTypecheck :: Expr -> TypeEnv -> ValOrErr Type
boolBinOpTypecheck (BinExpr _ e1 op e2) env = do
    t1 <- typecheckExpr e1 env
    t2 <- typecheckExpr e2 env

    case (t1, t2) of
        (BoolType, BoolType) -> Val BoolType
        otherwise            ->
            Err (makeTypeError [e1, e2] [t1, t2] op env)

bitOpTypecheck :: Expr -> TypeEnv -> ValOrErr Type
bitOpTypecheck (BinExpr _ a op b) env =
    do
        atype <- typecheckExpr a env
        btype <- typecheckExpr b env

        case (atype, btype) of
                (BitsType abits, BitsType bbits) ->
                    if abits == bbits
                        then Val (BitsType abits)
                        else
                            Err ((makeTypeError [a, b] [atype, btype] DivOp env)
                            ++ "because they do not have the same number of bits.")
                otherwise ->
                    Err ((makeTypeError [a, b] [atype, btype] DivOp env)
                    ++ "because they aren't both Bits")

equalityTypecheck :: Expr -> TypeEnv -> ValOrErr Type
equalityTypecheck (BinExpr _ a op b) env = do
    atype <- typecheckExpr a env
    btype <- typecheckExpr b env

    case alignTypesStrict atype btype of
        Nothing -> Err (makeTypeError [a, b] [atype, btype] op env)
        Just t  -> Val BoolType
