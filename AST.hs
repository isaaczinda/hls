module AST where

import ParserBase

data Type =
        BoolType |
        UIntType Int |
        IntType Int | --
        FixedType Int Int | -- integer bits, decimal bits
        BitsType Int
    deriving (Eq)

instance Show Type where
    show t = case t of
        BoolType      -> "Bool"
        UIntType x    -> "UInt" ++ (show x)
        IntType x     -> "Int" ++ (show x)
        FixedType x y -> "Fixed" ++ (show x) ++ "." ++ (show y)
        BitsType x    -> "Bits" ++ (show x)

data Literal =
        Dec Int |
        Fixed Int Int |
        Bin String |
        Hex String
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
    deriving (Show, Eq)

data UnOp = BitNotOp | NotOp | NegOp
    deriving (Show, Eq)


data Expr =
        -- binary arithmatic: +, -, *, \, &,
        BinExpr ParseString Expr BinOp Expr |

        -- unary operations: ~, !
        UnExpr ParseString UnOp Expr |

        -- array or bit slice: a[1..2]
        Slice ParseString Expr Int Int |

        -- array or bit index: a[3]
        Index ParseString Expr Int |

        Exactly ParseString Literal |

        Variable ParseString Var
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
