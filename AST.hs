module AST where

import ParserBase

data Type =
        BoolType |
        UIntType Int |
        IntType Int | --
        FixedType Int Int | -- integer bits, decimal bits
        BitsType Int |
        ListType Type Int
    deriving (Eq)

instance Show Type where
    show t = case t of
        BoolType      -> "Bool"
        UIntType x    -> "UInt" ++ (show x)
        IntType x     -> "Int" ++ (show x)
        FixedType x y -> "Fixed" ++ (show x) ++ "." ++ (show y)
        BitsType x    -> "Bits" ++ (show x)
        ListType x y  -> (show x) ++ "[" ++ (show y) ++ "]"

data Literal =
        Dec Int |
        Fixed String |
        Bin String |
        Hex String |
        Bool Bool
    deriving (Show, Eq)

type Var = String

data BinOp =
        PlusOp |
        TimesOp |
        MinusOp |
        DivOp |

        BitAndOp |
        BitXOrOp |
        BitOrOp |

        ConcatOp |

        EqualsOp |
        NotEqualsOp |
        OrOp |
        AndOp
    deriving (Eq)

instance Show BinOp where
    show t = case t of
            PlusOp -> "+"
            TimesOp -> "*"
            MinusOp -> "-"
            DivOp -> "/"
            BitAndOp -> "&"
            BitXOrOp -> "^"
            BitOrOp -> "|"
            ConcatOp -> "++"
            EqualsOp -> "=="
            NotEqualsOp -> "!="
            OrOp -> "||"
            AndOp -> "&&"

instance Show UnOp where
    show t = case t of
            BitNotOp -> "~"
            NotOp    -> "!"
            NegOp    -> "-"


data UnOp = BitNotOp | NotOp | NegOp
    deriving (Eq)


data Expr =
        BinExpr ParseString Expr BinOp Expr | -- binary arithmatic
        UnExpr ParseString UnOp Expr | -- unary operations
        Slice ParseString Expr Int Int | -- array or bit slice: a[1..2]
        Index ParseString Expr Int | -- array or bit index: a[3]
        Exactly ParseString Literal |
        Variable ParseString Var |
        Cast ParseString Type Expr |
        List ParseString [Expr]
    deriving (Show, Eq)

getParseString :: Expr -> ParseString
getParseString e =
    case e of
            (BinExpr s _ _ _) -> s
            (UnExpr s _ _)    -> s
            (Slice s _ _ _)   -> s
            (Index s _ _)     -> s
            (Exactly s _)     -> s
            (Variable s _)    -> s
            (Cast s _ _)      -> s
            (List s _)        -> s

setParseString :: Expr -> ParseString -> Expr
setParseString e s =
    case e of
            (BinExpr _ a b c) -> (BinExpr s a b c)
            (UnExpr _ a b)    -> (UnExpr s a b)
            (Slice _ a b c)   -> (Slice s a b c)
            (Index _ a b)     -> (Index s a b)
            (Exactly _ a)     -> (Exactly s a)
            (Variable _ a)    -> (Variable s a)
            (Cast _ a b)      -> (Cast s a b)
            (List _ a)        -> (List s a)

-- combines two parse strings so that the area between their extreme bounds
-- is the new parse string
combParseStrings :: ParseString -> ParseString -> ParseString
combParseStrings (s1, e1) (s2, e2) = (s, e)
    where
        s = if s1 < s2 then s1 else s2
        e = if e1 > e2 then e1 else e2
