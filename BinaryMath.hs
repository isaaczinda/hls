module BinaryMath where
import AST
import Data.List.Split
import Misc (dropLast, repeatChar)

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

multiplyFixed :: Type -> Type -> String -> String -> String
multiplyFixed (FixedType i1 d1) (FixedType i2 d2) v1 v2 =
        res''
    where

        reslen = i1 + d1 + i2 + d2 -- the length of the final result
        decdiff = abs (d1 - d2)

        -- long enough so that nothing gets truncated
        -- strictly longer than res length
        inputlen = (max i1 0) + (max i2 0) + d1 + d2 -- the length of both of the inputs

        -- this adjustment WORKS
        -- adjust strings to have the same number of decimal digits
        (v1', v2') -- "1", "1"
            | d1 >= d2 = (v1, (fracExtend v2 decdiff))
            | d1 < d2  = ((fracExtend v2 decdiff), v2)

        --
        -- adjust strings to have the same number of digits as the result
        -- will have
        v1intextra = inputlen - (length v1') -- digits to add to v1 -- 1
        v2intextra = inputlen - (length v2') -- digits to add to v2 -- 1

        v1'' = intExtend v1' v1intextra True
        v2'' = intExtend v2' v2intextra True

        -- res = error ((show v1') ++ " " ++ (show v2') ++ ", " ++ (show v1'') ++ " " ++ (show v2''))

        res = multiplySameLen v1'' v2''
        res' = (fracExtend res (-decdiff))
        res'' = intExtend res' (-(v1intextra + v2intextra)) True

multiply :: Bool -> String -> String -> String
multiply issigned v1 v2 = res
    where
        -- the target length of the result
        targetlen = (length v1) + (length v2)

        -- extend both numbers so that they are themselves the size of the
        -- result
        v1' = intExtend v1 (targetlen - (length v1)) issigned
        v2' = intExtend v2 (targetlen - (length v2)) issigned

        rawres = multiplySameLen v1' v2'
        res = intExtend rawres (targetlen - (length rawres)) False

-- multiply two inputs of the same length
multiplySameLen :: String -> String -> String
multiplySameLen v1 v2 =
        addStrings (setupMult 0)
    where
        -- convert multiplication problem to an addition of strings problem
        -- position in v1 (starts at 0) we are at
        setupMult :: Int -> [String]
        setupMult pos
            | pos >= (length v1) = []
            | otherwise                = prod ++ (setupMult (pos + 1))
            where
                v1char = (reverse v1)!!pos
                -- product of v1char and v2
                prod = case v1char of
                        '0' -> ["0"]
                        '1' -> [v2 ++ (repeatChar '0' pos)]


-- add many binary strings together, which may all be different lengths
-- the strings will be padded with zeroes before adding, so they should
-- probably be unsigned
addStrings :: [String] -> String
addStrings values =
        foldl1 alignThenAdd values
    where
        alignThenAdd :: String -> String -> String
        alignThenAdd v1 v2 = add v1' v2'
            where
                (v1', v2') = alignLengths v1 v2

alignLengths :: String -> String -> (String, String)
alignLengths s1 s2
    -- if s1 is longer
    | (length s1) >= (length s2) = (s1, (repeatChar '0' diff) ++ s2)
    -- if s2 is longer
    | otherwise                  = ((repeatChar '0' diff) ++ s1, s2)

    where
        diff = abs ((length s1) - (length s2))

-- add two binary strings of the same size
-- the result should be one bit longer than the length of the input strings
add :: String -> String -> String
add v1 v2 =
        if length v1 == length v2
            then addHelper v1 v2 '0'
            else error "inputs to add must be the same length"
    where
        addHelper :: String -> String -> Char -> String
        addHelper "" "" carry = [carry]
        addHelper v1 v2 carry =
                (addHelper v1' v2' carry') ++ [bitVal]
            where
                v1' = dropLast v1
                v2' = dropLast v2
                (carry', bitVal) = fullAdder (last v1) (last v2) carry

        -- output is (carry, value)
        fullAdder :: Char -> Char -> Char -> (Char, Char)
        fullAdder a b c =
                case bitSum of
                    0 -> ('0', '0')
                    1 -> ('0', '1')
                    2 -> ('1', '0')
                    3 -> ('1', '1')
            where
                bitSum = (charToInt a) + (charToInt b) + (charToInt c)

        charToInt :: Char -> Int
        charToInt '0' = 0
        charToInt '1' = 1

-- output string is one larger than input string, due to negative overflow
negative :: String -> String
negative v =
        add vnot one
    where
        vnot = bitNot v
        one = (repeatChar '0' ((length v) - 1)) ++ "1"

bitAnd :: String -> String -> String
bitAnd "" "" = ""
bitAnd ('1':r1) ('1':r2) = '1':(bitAnd r1 r2)
bitAnd (_:r1) (_:r2) = '0':(bitAnd r1 r2)
bitAnd _ _ = error "inputs to bitAnd must be the same length"

bitNot :: String -> String
bitNot "" = ""
bitNot ('1':r) = '0':(bitNot r)
bitNot ('0':r) = '1':(bitNot r)

bitOr :: String -> String -> String
bitOr a b =
        if length a == length b
            then result
            else error "inputs to bitOr must be the same length"
    where
        -- ~(~a & ~b) == a | b
        result = bitNot (bitAnd (bitNot a) (bitNot b))

bitXOr :: String -> String -> String
bitXOr a b =
    if length a == length b
        then result
        else error "inputs to bitXOr must be the same length"
    where
        -- true when exactly 1 of the two bits is high
        -- (a | b) & ~(a & b)
        oneHigh = bitOr a b
        bothHigh = bitAnd a b
        result = bitAnd oneHigh (bitNot bothHigh)

binToUInt :: String -> Int
binToUInt v = binToUIntHelper v 0
    where


        binToUIntHelper :: String -> Int -> Int
        binToUIntHelper "" _ = 0 -- base case
        binToUIntHelper v place =
                val + binToUIntHelper (dropLast v) (place + 1)
            where
                val = case (last v) of
                    '0' -> 0
                    '1' -> 2 ^ place

intToBin :: Int -> String
intToBin int
    -- calculate a power of two that is >= int,
    | int < 0   = "1" ++ uintToBinHelper (msbVal + int) (bits-1)
    | otherwise = "0" ++ uintToBinHelper int (bits-1)
    where
        bits = intBits int
        -- (positive) value of most significant bit in int
        msbVal = (2^(bits-1))


-- to be used only as a helper function
-- returns that 0 can be represented in 0 bits, which is technically correct
-- but not very useful
minUnsignedBits :: Int -> Int
minUnsignedBits x
    | x < 0     = error "negative number cannot be represented using unsigned"
    | otherwise = ceiling (logBase 2 (fromIntegral (x + 1)))

-- gets the number of bits needed to hold an unsigned positive number
uintBits :: Int -> Int
uintBits x
    | x == 0    = 1
    | otherwise = minUnsignedBits x

intBits :: Int -> Int
intBits x
    | x >= 0    = (minUnsignedBits x) + 1
    -- x - 1 because negative numbers are allowed to be 1 larger than
    -- positive numbers because of two's complement
    | otherwise = (minUnsignedBits ((abs x) - 1)) + 1

uintToBin :: Int -> String
uintToBin int = uintToBinHelper int startingBit
    where
        -- 1 is the LSB
        startingBit = (uintBits int)

-- represent unsigned number as a string with the specified number
-- of binary digits
uintToBinHelper :: Int -> Int -> String
uintToBinHelper int bits
    | bits == 0 = ""
    | int-bitVal >= 0 = "1" ++ (uintToBinHelper (int - bitVal) (bits - 1))
    | otherwise       = "0" ++ (uintToBinHelper int (bits - 1))

    where bitVal = 2 ^ (bits - 1)

fixedToBin :: String -> String
fixedToBin s = fst (fixedHelper s)

fixedHelper :: String -> (String, Type)
fixedHelper str
    | val == 0  = ("0", (FixedType 1 0))
    | otherwise = (repr, (FixedType intBits fracBits))

    where
        val = (read str) :: Double
        fracStr = last (splitOn "." str)

        -- given the trailing zeroes, calculate the minimum error we can have
        -- if the number if .012, the max error is .0005
        maxError = 1 / (10^(length fracStr) * 2)

        -- find the MSB (same as integer bits)
        -- 1 means that we use 1 digit to the right of the decimal
        -- 2 means that we use 2 digits to the right of the decimal
        intBits
            -- for cases where -4 and -3.999 take different number of integer
            -- bits to represent
            | val > 0 && isPower = ceiling (logBase 2 val) + 2

            -- for cases where -4 and -3.9999 take the same amount of integer
            -- bits to represent
            | otherwise = ceiling (logBase 2 (abs val)) + 1

            where
                logVal = logBase 2 val
                isPower = fromIntegral (floor logVal) - logVal == 0


        msbVal = -(2^^(intBits-1))
        -- msbVal = error (show intBits)

        -- calculate the value of the negative bit, as well as the new positive
        -- value that we have to compute
        (msbStr, posVal)
            | val < 0 = ("1", val - msbVal)
            | val > 0 = ("0", val)

        -- use val' and msb-1 since we've already used 1 bit
        repr = msbStr ++ (fracToBin posVal (intBits - 1))

        -- if the string is 2 long and there are -2 int bits, then the
        -- representaiton must extend to 4 bits right of the decimal
        fracBits = (length repr) - intBits

        -- value (must be positive), integer bits
        fracToBin :: Double -> Int -> String
        fracToBin value msb
                -- if we need to keep adding bits
                | value > maxError || msb >= 1 = bitStr ++ (fracToBin value' (msb - 1))

                -- if we have low enough error and have filled all integer bits
                | otherwise        = ""
            where
                -- the value that putting a 1 in the next place would add to
                -- the number
                placeValue = (2 ^^ (msb - 1))


                bitUsed = (value - placeValue) >= 0
                bitStr = if bitUsed then "1" else "0"
                value' = if bitUsed then value - placeValue else value
