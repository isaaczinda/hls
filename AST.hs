module AST where

import ParserBase

data Type =
        BoolType |
        UIntType Int |
        IntType Int | --
        FixedType Int Int | -- integer bits, decimal bits
        BitsType Int
    deriving (Show, Eq)

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


-- a pair of the AST expression and the underlying string
type ExprString = (Expr, ParseString)

data Expr =
        -- binary arithmatic: +, -, *, \, &,
        BinExpr ExprString BinOp ExprString |

        -- unary operations: ~, !
        UnExpr UnOp ExprString |

        -- array or bit slice: a[1..2]
        Slice ExprString Int Int |

        -- array or bit index: a[3]
        Index ExprString Int |

        Exactly Literal |

        Variable Var

    deriving (Show, Eq)
