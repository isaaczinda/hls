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
string s@(h:t) = ((char h) <:> (string t)) <??> ("expected " ++ s)

--
-- string_ws :: String -> Parser String
-- string_ws = \s -> makeWs (string s)
--
-- -- Takes list of string and creates a string parser where unlimited whitespace
-- -- is allowed between before and after each string.
-- strings_ws :: [String] -> Parser [String]
-- strings_ws strings = comb_parser
--     where
--         -- reverse the input list so that the first elements in original list
--         -- are last to be folded in, and and are thus matched first.
--         parsers = map string_ws (reverse strings)
--
--         -- make the first parser into a list parser instead of a string parser
--         fst_str_parser = (head parsers) >>= \x -> return [x]
--
--         -- fold all the parsers together into an
--         -- (flip (<:>)) makes it so <:> takes arguments in this order:
--         --     pstringlist, pstring
--         comb_parser = foldl (flip (<:>)) fst_str_parser (tail parsers)

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
literalFixed = (ufixed >>=: \(a, b) -> (Fixed a b)) <??> "expected fixed point literal"

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

-- parse a literal value
literal :: Parser Literal
literal = (literalBin <|> literalHex <|> literalFixed <|> literalDec) <??> "expected a literal"


-- TYPES

-- parses a type
parseType :: Parser Type
parseType = (intType <|> fixedType <|> bitsType <|> boolType) <??> "expected a variable type"

-- parses integer type keyword, eg. Int9
intType :: Parser Type
intType = (string "Int" <-+> uint) >>= \x -> return (IntType x)

-- parses fixed type keyword, eg. Fixed9.2
fixedType :: Parser Type
fixedType = (string "Fixed" <-+> ufixed) >>= \(a, b) -> return (FixedType a b)

-- parses bits type keyword, eg. Bits9.2
bitsType :: Parser Type
bitsType =
    (string "Bits") <-+> uint
    >>= \x -> (return (BitsType x))

boolType :: Parser Type
boolType = (string "Bool") >>=: \x -> BoolType


{-
A slice operation works on arrays and datatypes. There may be any number of
index operations -- since there can be arbitrarily deep arrays -- followed by
a single slice operation.

a[1][4][2..3] works on a 2D array of Bits4.
-}
--


makeBinExpr :: BinOp -> (ExprString -> ExprString -> ExprString)
makeBinExpr op = \e1@(_, (start_pos, _)) e2@(_, (_, end_pos)) -> ((BinExpr e1 op e2), (start_pos, end_pos))

-- chaine1 :: Parser Expr -> Parser a ->
--
-- chaine1 :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser a
-- chaine1 p op =
--     p1 <+> many (op <+> p)
--     >>=: \(head,oprs) -> foldl (\a (f,rest) -> f a rest) head oprs
--
--     where
--         -- first parser
--
--         p1 = p >>>= \x s return (x, s)


data Tree = A {t :: Int, b :: Int} | B {t :: Int}

getT :: Tree -> Int
getT x = (t x)

-- ^^^ delete this
--
-- Named Fields (Record Syntax)
--
-- use original chainl1, with a new combined function to do Expr -> Expr -> Expr

--
expr :: Parser ExprString
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
--
bitandop = addws (string "&" >>=: \x -> BitAndOp)
bitorop = addws (string "|" >>=: \x -> BitOrOp)
bitxorop = addws (string "^" >>=: \x -> BitXOrOp)

concatop = addws (string "++" >>=: \x -> ConcatOp)

-- parser to parse && and || operations
andorfactor :: Parser ExprString
andorfactor = chainl1 equalsfactor andor
    where
        -- If the first part of the expression parses and there is a second part
        -- which doesn't match (eg. a[1]78), we will get the "expected operator"
        -- error.
        andor :: Parser (ExprString -> ExprString -> ExprString)
        andor = ((orop <|> andop) >>=: makeBinExpr) <???> "expected operator"

-- parser to parse == and != operations
equalsfactor :: Parser ExprString
equalsfactor = chainl1 mathfactor equality
    where
        equality :: Parser (ExprString -> ExprString -> ExprString)
        equality = (equalsop <|> notequalsop) >>=: makeBinExpr

-- parser to parse +, -, *, and / operations
mathfactor :: Parser ExprString
mathfactor = chainl1 bitfactor mathops
    where
        mathops :: Parser (ExprString -> ExprString -> ExprString)
        mathops =
            (plusop <|> minusop <|> divop <|> timesop) >>=: makeBinExpr



-- parser to parse &, |, ^, or ++
bitfactor :: Parser ExprString
bitfactor = chainl1 notfactor bitops
    where
        bitops :: Parser (ExprString -> ExprString -> ExprString)
        bitops =
            (bitandop <|> bitorop <|> bitxorop <|> concatop) >>=: makeBinExpr



-- parser to parse ~ and !
notfactor :: Parser ExprString
notfactor =
        negexpr <|> bitnotexpr <|> notexpr <|> selectfactor
    where
        bitnotop = addws (string "~")
        notop = addws (string "!")
        negop = addws (string "-")

        bitnotexpr = (bitnotop <-+> notfactor) -->: \x -> (UnExpr BitNotOp x)
        notexpr = (notop <-+> notfactor) -->: \x -> (UnExpr NotOp x)
        negexpr = (negop <-+> notfactor) -->: \x -> (UnExpr NegOp x)


-- parser to parse select operations: a[1] (index) and a[1..2] (slice)
selectfactor :: Parser ExprString
selectfactor =
        basefactor <+> many index <+> optional slice
        --> \((base, indices), slice) (start_pos, _) -> -- keep track of entire match start
            let
                foldFunc = \e (i, (_, end_pos)) -> ((Index e i), (start_pos, end_pos))
                indexExpr = (foldl foldFunc base indices)
            in case slice of
                Just ((i1, i2), (_, end_pos)) -> return ((Slice indexExpr i1 i2), (start_pos, end_pos))
                Nothing -> return indexExpr

    where
        slice :: Parser ((Int, Int), ParseString)
        slice =
            (char '[' <-+> (addws uint) <+-> string "..") <+>
            ((addws uint) <+-> char ']')
                -->: \x -> x

        index :: Parser (Int, ParseString)
        index = (char '[' <-+> uint <+-> char ']')
            -->: \x -> x


basefactor :: Parser ExprString
basefactor = (literalexpr <|> varexpr <|> parensexpr) <???> "expected an expression"
    where
        parensexpr = (addws (char '(')) <-+> expr <+-> (addws (char ')'))
        literalexpr = literal -->: \x -> (Exactly x)
        varexpr = var -->: \x -> (Variable x)
