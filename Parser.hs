-- just export the expression parser
module Parser (expr) where

import ParserBase
import Data.Char
import AST



-- (char c) matches the char c
char :: Char -> Parser Char
char c = (get <=> \x -> x == c) <??> ("expected " ++ [c])

-- match the string exactly!
string :: String -> Parser String
string "" = return ""
string s@(h:t) = ((char h) <:> (string t))


digit :: Parser Char
digit = (get <=> isDigit) <??> "expected digit"

letter :: Parser Char
letter = (get <=> isLetter) <??> "expected letter"

-- digits must be at least 1 digit long
digits :: Parser String
digits = digit <:> (many digit)

alphanum :: Parser Char
alphanum = digit <|> letter

ws :: Parser String
ws = ws_char <:> (many ws_char)
    where ws_char = ((char ' ') <|> (char '\t') <|> (char '\n'))

maybews :: Parser (Maybe String)
maybews = optional ws

-- -- make a parser into one which skips whitespace before running
-- makeWs :: Parser a -> Parser a
-- makeWs p = ws <-+> p


-- TODO: fill this in
isNotReserved :: String -> Bool
isNotReserved s = (not (elem s reserved))
    where
        reserved = ["Int", "Fixed", "Bits"]


-- Parses a variable which may contain 0-9 a-z _
var :: Parser Var
var = ((letter <:> (many (alphanum <|> (char '_')))) <=> isNotReserved) <??> "expected variable name"


-- parses a number (positive or negative) which MUST have a decimal point
fixed :: Parser (Int, Int)
fixed =
        (int <+-> (string ".")) <+> uint
        >>= \(intDigits, decDigits) -> return (intDigits, decDigits)

-- parses a positive number which must have a decimal point in it
ufixed :: Parser (Int, Int)
ufixed =
        (uint <+-> (string ".")) <+> uint
        >>= \(intDigits, decDigits) -> return (intDigits, decDigits)

int :: Parser Int
int =
    (optional (char '-')) <+> uint >>=:
        \(s, x) ->
            case s of
                (Just _) -> (x * (-1))
                Nothing -> x


uint :: Parser Int
uint = digits >>=: \x -> stringToInt x
    where
        stringToInt :: String -> Int
        stringToInt = read


-- LITERALS

literalFixed :: Parser Literal
literalFixed =
    do
        (a, b) <- ufixed
        let fixedStr = (show a) ++ "." ++ (show b)
        return (Fixed fixedStr)


literalDec :: Parser Literal
literalDec = uint >>=: \x -> (Dec x)

literalBin :: Parser Literal
literalBin =
        (string "0b") <-+> (many bit) >>=: \x -> (Bin x)
    where
        bit :: Parser Char
        bit = char '0' <|> char '1'

-- save in lowercase representation
literalHex :: Parser Literal
literalHex =
        (string "0x") <-+> (many hex) >>=: \x -> (Hex (map toLower x))
    where
        hex :: Parser Char
        hex = digit <|> (foldl1 (<|>) (map charNoCase ['a', 'b', 'c', 'd', 'e', 'f']))

        charNoCase :: Char -> Parser Char
        charNoCase c = char (toLower c) <|> char (toUpper c)

literalBool :: Parser Literal
literalBool = literalTrue <|> literalFalse
    where
        literalTrue = (string "true") >>=: \x -> (Bool True)
        literalFalse = (string "false") >>=: \x -> (Bool False)

-- parse a literal value
literal :: Parser Literal
literal = (literalBin <|> literalHex <|> literalFixed <|> literalDec <|> literalBool) <???> "expected a literal"


-- TYPES

-- parses a type
parseType :: Parser Type
parseType = (intType <|> fixedType <|> bitsType <|> boolType) <???> "expected a type"

-- parses integer type keyword, eg. Int9
intType :: Parser Type
intType = (string "Int" <-+> uint) >>= \x -> return (IntType x)

-- parses fixed type keyword, eg. Fixed9.2 or Fixed-2.3
fixedType :: Parser Type
fixedType =
    do
        (a, b) <- (string "Fixed" <-+> fixed)
        if a + b > 0
            then return (FixedType a b)
            else fail ("fixed type " ++ (show (FixedType a b)) ++ " has <=0 bits")

-- parses bits type keyword, eg. Bits9.2
bitsType :: Parser Type
bitsType =
    (string "Bits") <-+> uint
    >>= \x -> (return (BitsType x))

boolType :: Parser Type
boolType = (string "Bool") >>=: \x -> BoolType


makeBinExprCombiner :: BinOp -> (Expr -> Expr -> Expr)
makeBinExprCombiner op = binExprCombiner
    where
        binExprCombiner e1 e2 = BinExpr (start_pos, end_pos) e1 op e2
            where
                (start_pos, _) = getParseString e1
                (_, end_pos) = getParseString e2


--
expr :: Parser Expr
expr = andorfactor


-- parsers for all operations
addws :: Parser a -> Parser a
addws p = maybews <-+> p <+-> maybews

orop = addws (string "||" >>=: \x -> OrOp)
andop = addws (string "&&" >>=: \x -> AndOp)
equalsop = addws (string "==" >>=: \x -> EqualsOp)
notequalsop = addws (string "!=" >>=: \x -> NotEqualsOp)

plusop = addws (string "+" >>=: \x -> PlusOp)
minusop = addws (string "-" >>=: \x -> MinusOp)
timesop = addws (string "*" >>=: \x -> TimesOp)
divop = addws (string "/" >>=: \x -> DivOp)

bitandop = addws (string "&" >>=: \x -> BitAndOp)
bitorop = addws (string "|" >>=: \x -> BitOrOp)
bitxorop = addws (string "^" >>=: \x -> BitXOrOp)

concatop = addws (string "++" >>=: \x -> ConcatOp)

-- parser to parse && and || operations
andorfactor :: Parser Expr
andorfactor = chainl1 equalsfactor andor
    where
        andor :: Parser (Expr -> Expr -> Expr)
        andor = (orop <|> andop) >>=: makeBinExprCombiner


-- parser to parse == and != operations
equalsfactor :: Parser Expr
equalsfactor = chainl1 mathfactor equality
    where
        equality :: Parser (Expr -> Expr -> Expr)
        equality = (equalsop <|> notequalsop) >>=: makeBinExprCombiner


-- parser to parse +, -, *, and / operations
mathfactor :: Parser Expr
mathfactor = chainl1 bitfactor mathops
    where
        mathops :: Parser (Expr -> Expr -> Expr)
        mathops =
            (plusop <|> minusop <|> divop <|> timesop) >>=: makeBinExprCombiner


-- parser to parse &, |, ^, or ++
bitfactor :: Parser Expr
bitfactor = chainl1 notfactor bitops
    where
        bitops :: Parser (Expr -> Expr -> Expr)
        bitops =
            (bitandop <|> bitorop <|> bitxorop <|> concatop) >>=: makeBinExprCombiner


-- parser to parse -, ~, ! and explicit casting
notfactor :: Parser Expr
notfactor =
        negexpr <|> bitnotexpr <|> notexpr <|> castexpr <|> selectfactor
    where
        bitnotop = addws (string "~")
        notop = addws (string "!")
        negop = addws (string "-")
        castop = (addws (string "(")) <-+> parseType <+-> (addws (string ")"))

        bitnotexpr = (bitnotop <-+> notfactor) --> \x s -> return (UnExpr s BitNotOp x)
        notexpr = (notop <-+> notfactor) --> \x s -> return (UnExpr s NotOp x)
        negexpr = (negop <-+> notfactor) --> \x s -> return (UnExpr s NegOp x)
        castexpr = (castop <+> notfactor) --> \(t, e) s -> return (Cast s t e)

{-
A slice operation works on arrays and datatypes. There may be any number of
index operations -- since there can be arbitrarily deep arrays -- followed by
a single slice operation.

a[1][4][2..3] works on a 2D array of Bits4.
parser to parse select operations: a[1] (index) and a[1..2] (slice)
-}

selectfactor :: Parser Expr
selectfactor =
        basefactor <+> many index <+> optional slice
        --> \((base, indices), slice) (start_pos, _) -> -- keep track of entire match start
            let
                foldFunc = \e (i, (_, end_pos)) -> (Index (start_pos, end_pos) e i)
                indexExpr = (foldl foldFunc base indices)
            in case slice of
                Just ((i1, i2), (_, end_pos)) -> return (Slice (start_pos, end_pos) indexExpr i1 i2)
                Nothing -> return indexExpr

    where
        slice :: Parser ((Int, Int), ParseString)
        slice =
            (char '[' <-+> (addws uint) <+-> string "..") <+>
            ((addws uint) <+-> char ']')
                --> \x s -> return (x, s)

        index :: Parser (Int, ParseString)
        index = (char '[' <-+> uint <+-> char ']')
            --> \x s -> return (x, s)


basefactor :: Parser Expr
basefactor = (literalexpr <|> varexpr <|> parensexpr)
    where
        -- when we encounter parentheses that wrap an expression, we ignore the
        -- parentheses but modify the parse string so that it includes the
        -- parentheses
        parensexpr = ((addws (char '(')) <-+> selectfactor <+-> (addws (char ')')))
            --> \e s -> return (setParseString e s)

        literalexpr = literal --> \x s -> return (Exactly s x)
        varexpr = var --> \x s -> return (Variable s x)
