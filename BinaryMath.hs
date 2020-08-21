module BinaryMath where
import AST
import Data.List.Split
import Misc (dropLast)

repeatChar c 0 = ""
repeatChar c n = c:(repeatChar c (n-1))


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

intToBin :: Int -> String
intToBin int
    -- calculate a power of two that is >= int,
    | int < 0   = "1" ++ helper ((2^bits) + int) (bits-1)
    | otherwise = "0" ++ helper int (bits-1)
    where
        bits = intBits int

-- -- find a power of two greater than a certain number
intBits :: Int -> Int
intBits int
    | int < 0   = (floor (logBase 2 (fromIntegral (abs int)))) + 1
    | int == 0  = 1
    | otherwise = (uintBits int) + 1

-- find the largest power of two below a certain number
uintBits :: Int -> Int
uintBits num =
    case num of
        0 -> 1
        otherwise -> (floor (logBase 2 (fromIntegral num))) + 1

uintToBin :: Int -> String
uintToBin int = helper int startingBit
    where
        -- 1 is the LSB
        startingBit = (uintBits int)

helper :: Int -> Int -> String
helper int currentBit
    | currentBit == 0 = ""
    | int-bitVal >= 0 = "1" ++ (helper (int - bitVal) (currentBit - 1))
    | otherwise       = "0" ++ (helper int (currentBit - 1))

    where bitVal = 2 ^ (currentBit - 1)


fixedToBin :: String -> String
fixedToBin str = binrep
    where (ty, binrep) = fixedHelper str

-- input string may be any fixed point string (eg. "-12.3, 0.23, ...")
fixedHelper :: String -> (Type, String)
fixedHelper str =
        ((FixedType intBits fracBits), binRepr)
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
        intBin = intToBin intPart

        -- bits in integer part (+ 1) includes the sign as well
        intBits =
            if intPart == 0
                then -(leadingZeroes fracBin) + 1
                else (length intBin)
        fracBits = length fracBin -- bits in fractional part

        binRepr = if intBits < 0
            then drop intBits (intBin ++ fracBin)
            else (intBin ++ fracBin)

        leadingZeroes :: String -> Int
        leadingZeroes (h:rest)
             | h == '0'  = 1 + (leadingZeroes rest)
             | otherwise = 0
        leadingZeroes "" = 0


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
