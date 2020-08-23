module Interp where

import AST
import BinaryMath
import Data.Map (fromList, lookup)
import Misc

-- frame of variables mapped to types, code

-- type ValTy = (String, Type)


type Value = String
type ValEnv = Frame Value

interpExpr :: TExpr -> ValEnv -> Value

interpExpr (Exactly _ (Dec int)) _
    | int < 0   = intToBin int
    | otherwise = uintToBin int

interpExpr (Exactly _ (Hex digits)) _ =
        foldl1 (++) (map mapFunc digits)
    where
        lut = fromList [('0', "0000"), ('1', "0001"), ('2', "0010"), ('3', "0011"),
            ('4', "0100"), ('5', "0101"), ('6', "0110"), ('7', "0111"),
            ('8', "1000"), ('9', "1001"), ('a', "1010"), ('b', "1011"),
            ('c', "1100"), ('d', "1101"), ('e', "1110"), ('f', "1111")]

        mapFunc :: Char -> String
        mapFunc c = case Data.Map.lookup c lut of
            Just x  -> x
            Nothing -> error ("invalid character in hex: " ++ [c])

interpExpr (Exactly _ (Bin bits)) _ = bits

interpExpr (Exactly _ (Bool True)) _ = "1"
interpExpr (Exactly _ (Bool False)) _ = "0"

interpExpr (Exactly _ (Fixed str)) _ = bits
    where
        (bits, ty) = fixedHelper str

-- arithmatic operations

interpExpr (BinExpr resType e1 PlusOp e2) env =
        -- values are the same size by default
        add v1 v2
    where
        v1 = interpExpr e1 env
        v2 = interpExpr e2 env

interpExpr (BinExpr _ e1 MinusOp e2) env = result
    where
        v1 = interpExpr e1 env
        v2 = interpExpr e2 env

        -- make the second argument negative, extend the first argument
        -- by 1 bit
        v2neg = negative v2
        preserveSign = case (getExtra e1) of
            UIntType _ -> False
            otherwise -> True
        v1ext = intExtend v1 1 preserveSign

        -- result is just 1 bit larger than size of arguments (though
        -- add v2neg v1ext) is 2 bits larger
        (_:result) = add v2neg v1ext

interpExpr (UnExpr _ NegOp e) env =
    negative (interpExpr e env)


interpExpr (BinExpr _ e1 TimesOp e2) env =
    error "* not yet implemented"

interpExpr (BinExpr _ e1 DivOp e2) env =
    error "/ not yet implemented"

-- bitwise operations

interpExpr (UnExpr _ BitNotOp e) env =
    bitNot (interpExpr e env)

interpExpr (BinExpr _ e1 BitAndOp e2) env =
    bitAnd (interpExpr e1 env) (interpExpr e2 env)

interpExpr (BinExpr _ e1 BitXOrOp e2) env =
    bitXOr (interpExpr e1 env) (interpExpr e2 env)

interpExpr (BinExpr _ e1 BitOrOp e2) env =
    bitOr (interpExpr e1 env) (interpExpr e2 env)

-- boolean operations

interpExpr (UnExpr _ NotOp e) env =
    bitNot (interpExpr e env)

interpExpr (BinExpr _ e1 AndOp e2) env =
    bitAnd (interpExpr e1 env) (interpExpr e2 env)

interpExpr (BinExpr _ e1 OrOp e2) env =
    bitOr (interpExpr e1 env) (interpExpr e2 env)

-- equality checks

interpExpr (BinExpr _ e1 EqualsOp e2) env =
    if (interpExpr e1 env) == (interpExpr e2 env)
        then "1"
        else "0"

interpExpr (BinExpr _ e1 NotEqualsOp e2) env =
    if (interpExpr e1 env) /= (interpExpr e2 env)
        then "1"
        else "0"

-- list operations

interpExpr (List _ exprs) env = result
    where
        -- interpret the value of each item in the list
        vals = map (\e -> interpExpr e env) exprs

        -- concat values together, first in list farthest left then last
        -- farthest right
        result = foldl1 (++) vals

interpExpr (Index _ e index) env =
    sliceHelper e index index env

interpExpr (Slice _ e i1 i2) env =
    sliceHelper e i1 i2 env

interpExpr (BinExpr _ e1 ConcatOp e2) env =
    (interpExpr e1 env) ++ (interpExpr e2 env)

-- variables

interpExpr (Variable _ str) env = val
    where
        Just val = (getVar env str)

-- casting

interpExpr (Cast _ ty' e) env =
        castHelper val ty ty'
    where
        val = interpExpr e env
        ty = getExtra e
            -- cast anythign to bits type by

-- str, original type, new type
castHelper :: String -> Type -> Type -> String

-- * --> BitsType

castHelper val t1 (BitsType bits2) =
    if bitsInType t1 == bits2
        then val
        else error "can't cast to BitsType of different size"

-- BitsType --> *

castHelper val (BitsType bits1) t2 =
    if bitsInType t2 == bits1
        then val
        else error "can't cast from BitsType of different size"

-- UIntType --> *

castHelper val (UIntType bits1) (UIntType bits2) =
    intExtend val (bits2 - bits1) False

castHelper val (UIntType bits1) (IntType bits2) =
    intExtend val (bits2 - bits1) False

castHelper val (UIntType bits1) (FixedType i2 d2) =
        fracExtend val' d2
    where
        val' = intExtend val (i2 - bits1) False

-- IntType --> *

castHelper val (IntType bits1) (UIntType bits2) =
    intExtend val (bits2 - bits1) False

castHelper val (IntType bits1) (IntType bits2) =
    intExtend val (bits2 - bits1) True

castHelper val (IntType bits1) (FixedType i2 d2) =
        fracExtend val' d2
    where
        val' = intExtend val (i2 - bits1) True

-- FixedType --> *

castHelper val (FixedType i1 d2) (UIntType bits2) =
        fracExtend val' (-d2)
    where
        val' = intExtend val (bits2 - i1) False

castHelper val (FixedType i1 d1) (IntType bits2) =
        fracExtend val' (-d1)
    where
        val' = intExtend val (bits2 - i1) True

castHelper val (FixedType i1 d1) (FixedType i2 d2) =
        fracExtend val' (d2 - d1)
    where
        val' = intExtend val (i2 - i1) True

-- castHelper _ _ _ = throwIO MyException

-- inclusive
sliceHelper :: TExpr -> TExpr -> TExpr -> ValEnv -> String
sliceHelper e i1 i2 env =
        slice firstPointer lastPointer (interpExpr e env)
    where
        -- width of each element in indexed type
        elemWidth = case getExtra e of
            ListType t l -> bitsInType t
            BitsType _ -> 1

        i1val = read (interpExpr i1 env) :: Int
        i2val = read (interpExpr i2 env) :: Int

        -- the bit values of the start and end of the element that the index
        -- selects
        firstPointer = i1val * elemWidth
        lastPointer = (i2val + 1) * elemWidth - 1

{-
Allows extending of UInt, Int, and Fixed variables
value+type, integer bits to add, decimal bits to add
-}
-- extend :: ValTy -> Int -> Int
-- extend (v1, t1) extraIntBits extraDecBits =
--     case t1 t2 of
--         (UIntType _, UIntType _)   -> extendHelper False v1 v2
--         (IntType _, IntType _)     -> extendHelper True v1 v2
--         (FixedType i1 d1, FixedType i2 d2) ->
--             let (v1', v2') = fracExtend d1 d2 v1 v2
--             in extendHelper True v1 v2
--
--     where

intExtend :: String -> Int -> Bool -> String
intExtend v extraBits preserveSign
        | extraBits < 0  = drop (abs extraBits) v
        | extraBits >= 0 = (repeatChar char extraBits) ++ v
    where
        char = if preserveSign then (head v) else '0'

fracExtend :: String -> Int -> String
fracExtend v extraBits
    | extraBits >= 0 = v ++ (repeatChar '0' extraBits)
    | extraBits < 0  = take ((length v) + extraBits) v
