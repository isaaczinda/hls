module Typecheck where

import AST
import Parser

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


getType :: Expr -> Type

-- getType (Exactly (Dec a))

-- getType (Exactly (Fixed a b)) = FixedType

getType (Exactly (Bin a)) = BitsType (length a)

getType (Exactly (Hex a)) = BitsType ((length a) * 4)
