module Interp where

import AST
import BinaryMath
import Data.Map (fromList, lookup)


-- frame of variables mapped to types, code

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
        (_, bits) = fixedHelper str

-- interpExpr (BinOp restype e1 PlusOp e2) env =
--         add v1' v2'
--     where
--         v1 = interpExpr e1 env
--         v2 = interpExpr e2 env
--         t1 = getExtra e1
--         t2 = getExtra e2
--
--         -- extend the values so that they are the same size
--         (v1', v2') = extend t1 t2 v1 v2

repeatChar c n = c:(zeroes (n-1))
repeatChar c 0 = ""

-- get rid of all this and write cast first !


extend :: Type -> Type -> String -> String -> (String, String)
extend t1 t2 v1 v2 =
    case t1 t2 of
        (UIntType _, UIntType _)   -> extendHelper False v1 v2
        (IntType _, IntType _)     -> extendHelper True v1 v2
        (FixedType i1 d1, FixedType i2 d2) ->
            let (v1', v2') = fracExtend d1 d2 v1 v2
            in extendHelper True v1 v2

    where
        extendHelper :: Bool -> String -> String -> (String, String)
        extendHelper preserveSign v1 v2
            | diff == 0 = (v1, v2)
            | diff < 0  = ((repeatChar char (abs diff)) ++ v1, v2)
            | diff > 0  = (v1, (repeatChar char diff) ++ v2)

            where
                diff = (length v1) - (length v2)
                signChar
                    | diff < 0  = head v1
                    | otherwise = head v2

                char = if preserveSign then signChar else '0'

        fracExtend :: Int -> Int -> String -> String -> (String, String)
        fracExtend d1 d2 v1 v2
            | diff == 0 = (v1, v2)
            | diff < 0  = (v1 ++ (repeatChar '0' (abs diff)), v2)
            | diff > 0  = (v1, v2 ++ (repeatChar '0' diff))

            where
                diff = d1 - d2
