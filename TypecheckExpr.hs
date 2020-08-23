module TypecheckExpr (typecheckExpr) where

import AST
import TypecheckBase
import BinaryMath (fixedHelper, uintBits, intBits)

{-
Since expressions don't make any changes to the type environment, we don't need
to have the output of typecheckExpr contain a type environment.
-}
typecheckExpr :: PExpr -> TypeEnv -> ValOrErr TExpr

-- typecheck decimal literals
typecheckExpr (Exactly _ (Dec a)) _
    | a >= 0    = return (Exactly (UIntType (uintBits a)) (Dec a))
    | otherwise = return (Exactly (IntType (intBits a)) (Dec a))

-- typecheck fixed-point numbers
typecheckExpr (Exactly _ (Fixed str)) _ =
    do
        t <- typecheckFixed str
        return (Exactly t (Fixed str))

-- negative fixed-point numbers
typecheckExpr (UnExpr _ NegOp (Exactly _ (Fixed str))) _ =
    do
        t <- typecheckFixed (str)
        t' <- typecheckFixed ("-" ++ str)
        return (UnExpr t' NegOp (Exactly t (Fixed str)))

-- typecheck bin
typecheckExpr (Exactly _ (Bin a)) _ =
    return (Exactly (BitsType (length a)) (Bin a))
typecheckExpr (Exactly _ (Hex a)) _ =
    return (Exactly (BitsType ((length a) * 4)) (Hex a))

-- typecheck bool
typecheckExpr (Exactly _ (Bool val)) _ = return (Exactly BoolType (Bool val))

-- typecheck addition
typecheckExpr e@(BinExpr _ a PlusOp b) env = typecheckAddSub e env

-- typecheck subtraction
typecheckExpr e@(BinExpr _ a MinusOp b) env = typecheckAddSub e env


-- typecheck bitwise operations
typecheckExpr e@(BinExpr _ _ BitAndOp _) env = bitOpTypecheck e env
typecheckExpr e@(BinExpr _ _ BitOrOp _) env = bitOpTypecheck e env
typecheckExpr e@(BinExpr _ _ BitXOrOp _) env = bitOpTypecheck e env


-- typecheck multiplication
typecheckExpr (BinExpr _ a TimesOp b) env =
    do
        a' <- typecheckExpr a env
        b' <- typecheckExpr b env
        let atype = getExtra a'
        let btype = getExtra b'

        -- alignTypes brings the types into the same class so that HOPEFULLY
        -- we can do addition on them
        (t, atype', btype') <-
            case (alignTypes atype btype) of
                -- UIntX * UIntY = UInt(X+Y)
                Just (UIntType abits, UIntType bbits) ->
                    return (UIntType (abits + bbits), UIntType abits, UIntType bbits)
                -- IntX * IntY = Int(X+Y)
                -- (not (X+Y-1) since -8 (Int4) * -8 (Int4) != (Int7) which can
                -- represent 63 at most
                Just (IntType abits, IntType bbits) ->
                    return (IntType (abits + bbits), IntType abits, IntType bbits)
                Just (FixedType aint adec, FixedType bint bdec) ->
                    return (FixedType (aint + bint) (adec + bdec), FixedType aint adec, FixedType bint bdec)
                otherwise -> fail (makeOpTypeError [a, b] [atype, btype] TimesOp env)

        return (BinExpr t (Cast atype' atype' a') TimesOp (Cast btype' btype' b'))

-- typechecks division
typecheckExpr (BinExpr _ a DivOp b) env =
    do
        a' <- typecheckExpr a env
        b' <- typecheckExpr b env
        let atype = getExtra a'
        let btype = getExtra b'

        (t, atype', btype') <-
            case (alignTypes atype btype) of
                Just (UIntType abits, UIntType bbits) ->
                    return (UIntType abits, UIntType abits, UIntType bbits)
                Just (IntType abits, IntType bbits) ->
                    return (IntType abits, IntType abits, IntType bbits)
                Just (FixedType aint afrac, FixedType bint bfrac) ->
                    return (FixedType (aint + bfrac) afrac, FixedType aint afrac, FixedType bint bfrac)
                otherwise ->
                    fail (makeOpTypeError [a, b] [atype, btype] DivOp env)

        return (BinExpr t (Cast atype' atype' a') DivOp (Cast btype' btype' b'))

-- typecheck variables
typecheckExpr (Variable s var) env@(frame, code) =
    case getVar frame var of
        Just (ty, _) -> return (Variable ty var)
        Nothing -> fail (makeUndefVarErr s var)

-- typecheck explicit casting
-- an explicit cast modifies the type but MUST preserve the underlying number
-- of bits
typecheckExpr (Cast s t' e) env =
    do
        e' <- typecheckExpr e env
        let t = getExtra e'

        let isValid = case (t, t') of
                -- can cast to bits as long as both types are same length
                (_, BitsType bits) -> bitsInType t == bits
                -- can cast from bits as long as both types are same length
                (BitsType bits, _) -> bitsInType t' == bits
                -- can cast between any numeric types
                (t1, t2) -> isNumericType t1 && isNumericType t2

        if isValid
            then return (Cast t' t' e')
            else fail (makeCastError e t t' env)

    where
        isNumericType (UIntType _) = True
        isNumericType (IntType _) = True
        isNumericType (FixedType _ _) = True
        isNumericType _ = False

-- typecheck negative operator
typecheckExpr (UnExpr s NegOp e) env =
    do
        e' <- typecheckExpr e env
        let etype = getExtra e'

        t <- case etype of
            (UIntType bits) -> return (IntType (bits + 1))
            (IntType bits) -> return (IntType (bits + 1))
            (FixedType intbits decbits) -> return (FixedType (intbits + 1) decbits)
            otherwise -> fail (makeOpTypeError [e] [etype] NegOp env)

        return (UnExpr t NegOp e')

-- typecheck list construction
typecheckExpr (List _ []) _ = return (List EmptyListType [])

typecheckExpr (List s exprs) env =
    do
        let sfirst = getExtra (head exprs)
        exprfirst <- typecheckExpr (head exprs) env
        let tfirst = getExtra exprfirst

        -- try to cast all expressions in the list declaration to the same
        -- type, and convert SExpr --> TExpr
        (_, elemtype, exprs') <- foldl comb (return (sfirst, tfirst, [exprfirst])) (tail exprs)

        -- explicit cast all of the expressions that constructed the list
        let exprs'' = map (\x -> (Cast elemtype elemtype x)) exprs'

        -- the type of the list
        let listtype = (ListType elemtype (length exprs))

        return (List listtype exprs'')
    where
        tryAlignType :: Type -> Type -> ParseString -> ParseString -> ValOrErr (ParseString, Type)
        tryAlignType t1 t2 s1 s2 =
            case (commonSupertype t1 t2) of
                Just t' ->
                    let s' = combParseStrings s1 s2
                    in return  (s', t')
                Nothing ->
                    let
                        -- because snippetMsg takes expressions, we
                        -- have to make fake expressions in order to
                        -- use it
                        snippet1 = snippetMsg (List s1 []) t1 env -- the list
                        snippet2 = snippetMsg (List s2 []) t2 env -- the single item
                    in
                        fail ("when constructing list, types of elements " ++ snippet1 ++ " and element " ++ snippet2 ++ " were incompatible")

        comb :: (ValOrErr (ParseString, Type, [TExpr]) -> PExpr -> ValOrErr (ParseString, Type, [TExpr]))
        comb current pexpr = do
            -- unpack the current 1) parse string, 2) type of
            -- list elements, 3) TExpr list
            (s, t, texprs) <- current

            texpr <- typecheckExpr pexpr env
            let snext = getExtra pexpr
            let tnext = getExtra texpr

            (s', t') <- tryAlignType t tnext s snext

            return (s', t', texpr:texprs)


-- typecheck indexing
typecheckExpr (Index _ e i) env = do
    i' <- typecheckIndex i env
    e' <- typecheckExpr e env
    let etype = getExtra e'

    t <- case etype of
        (ListType t _) -> return t
        (BitsType _)   -> return (BitsType 1)
        otherwise      -> fail ("cannot index non-list or bits type: " ++ (snippetMsg e etype env))

    return (Index t e' i')


-- typecheck slicing
typecheckExpr (Slice _ e i1 i2) env = do
    i1' <- typecheckImmediateIndex i1 env
    i2' <- typecheckImmediateIndex i2 env

    e' <- typecheckExpr e env
    let etype = getExtra e'

    t <- case etype of
        t@(ListType _ _) -> return t
        t@(BitsType _)   -> return t
        otherwise        -> fail ("cannot slice non-list or bits type: " ++ (snippetMsg e etype env))

    return (Slice t e' i1' i2')

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
    e' <- typecheckExpr e env
    let t = getExtra e'
    case t of
        BoolType  -> return (UnExpr BoolType NotOp e')
        otherwise -> fail (makeOpTypeError [e] [t] NotOp env)

-- typecheck ~
typecheckExpr (UnExpr _ BitNotOp e) env = do
    e' <- typecheckExpr e env
    let t = getExtra e'

    case t of
        (BitsType n)  -> return (UnExpr (BitsType n) BitNotOp e')
        otherwise -> fail (makeOpTypeError [e] [t] BitNotOp env)

-- typecheck ++
typecheckExpr (BinExpr _ e1 ConcatOp e2) env = do
    e1' <- typecheckExpr e1 env
    e2' <- typecheckExpr e2 env
    let e1type = getExtra e1'
    let e2type = getExtra e2'

    let err = fail (makeOpTypeError [e1, e2] [e1type, e2type] ConcatOp env)

    t <- case (e1type, e2type) of
        (BitsType n1, BitsType n2) -> return (BitsType (n1 + n2))

        -- handle cases where one or more of the lists are empty
        (EmptyListType, EmptyListType) -> return EmptyListType
        (EmptyListType, ListType t l) -> return (ListType t l)
        (ListType t l, EmptyListType) -> return (ListType t l)

        -- lists can only be concatenated when the elements the same type
        (ListType lt1 l1, ListType lt2 l2) ->
            if lt1 == lt2
                then return (ListType lt1 (l1 + l2))
                else err
        otherwise -> err

    return (BinExpr t e1' ConcatOp e2')


-- make sure that Expr is a UInt type to be used for indexing
typecheckIndex :: PExpr -> TypeEnv -> ValOrErr TExpr
typecheckIndex e env = do
    e' <- typecheckExpr e env
    let etype = getExtra e'

    case etype of
        (UIntType _) -> return e'
        otherwise -> fail ("index value " ++ (snippetMsg e etype env) ++ " is not a UInt")


-- make sure that Expr type is UInt, and that its value can be computed at
-- compile time
typecheckImmediateIndex :: PExpr -> TypeEnv -> ValOrErr TExpr
typecheckImmediateIndex e env = do
    e' <- typecheckIndex e env -- first make sure it's an index type

    let snippet = snippetMsg e (getExtra e') env

    -- now make sure that it's immediate
    case isImmdiate e of
        True  -> return e'
        False -> fail ("the value of the index " ++ snippet ++ " cannot be calculated at compile time")

typecheckFixed :: String -> ValOrErr Type
typecheckFixed str = return ty
    where (_, ty) = fixedHelper str

boolBinOpTypecheck :: PExpr -> TypeEnv -> ValOrErr TExpr
boolBinOpTypecheck (BinExpr _ e1 op e2) env = do
    e1' <- typecheckExpr e1 env
    e2' <- typecheckExpr e2 env
    let e1type = getExtra e1'
    let e2type = getExtra e2'

    case (e1type, e2type) of
        (BoolType, BoolType) -> return (BinExpr BoolType e1' op e2')
        otherwise ->
            fail (makeOpTypeError [e1, e2] [e1type, e2type] op env)

bitOpTypecheck :: PExpr -> TypeEnv -> ValOrErr TExpr
bitOpTypecheck (BinExpr _ a op b) env =
    do
        a' <- typecheckExpr a env
        b' <- typecheckExpr b env
        let atype = getExtra a'
        let btype = getExtra b'

        t <- case (atype, btype) of
            (BitsType abits, BitsType bbits) ->
                if abits == bbits
                    then return (BitsType abits)
                    else
                        fail ((makeOpTypeError [a, b] [atype, btype] op env)
                        ++ "because they do not have the same number of bits.")
            otherwise ->
                fail ((makeOpTypeError [a, b] [atype, btype] op env)
                ++ "because they aren't both Bits")

        return (BinExpr t a' op b')

equalityTypecheck :: PExpr -> TypeEnv -> ValOrErr TExpr
equalityTypecheck (BinExpr _ a op b) env = do
    a' <- typecheckExpr a env
    b' <- typecheckExpr b env
    let atype = getExtra a'
    let btype = getExtra b'

    case commonSupertype atype btype of
        Nothing -> fail (makeOpTypeError [a, b] [atype, btype] op env)
        Just t  -> return (BinExpr BoolType (Cast t t a') op (Cast t t b'))

typecheckAddSub :: PExpr -> TypeEnv -> ValOrErr TExpr
typecheckAddSub (BinExpr _ a op b) env =
    do
        a' <- typecheckExpr a env
        b' <- typecheckExpr b env
        let atype = getExtra a'
        let btype = getExtra b'

        (t, abtype') <-
            case (commonSupertype atype btype) of
                Just ty@(UIntType bits) ->
                    -- when subtracting UInt, we need to convert the result to Int
                    case op of
                        MinusOp -> return (IntType (bits + 1), (IntType (bits + 1)))
                        PlusOp   -> return (UIntType (bits + 1), ty)
                Just ty@(IntType bits) ->
                    return (IntType (bits + 1), ty)
                Just ty@(FixedType int dec) ->
                    return ((FixedType (int + 1) dec), ty)
                otherwise ->
                    fail (makeOpTypeError [a, b] [atype, btype] op env)

        -- now that we've confirmed that the types both work, add the implcit
        -- cast to the AST
        return (BinExpr t (Cast abtype' abtype' a') op (Cast abtype' abtype' b'))
