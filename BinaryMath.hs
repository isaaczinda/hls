module BinaryMath where
import AST
import Data.List.Split

-- assumes both input strings are of the same size
add :: String -> String -> String
add =



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
