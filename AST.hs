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


data ParseExpr =
        -- binary arithmatic: +, -, *, \, &,
        ParseBinExpr ParseString ParseExpr BinOp ParseExpr |

        -- unary operations: ~, !
        ParseUnExpr ParseString UnOp ParseExpr |

        -- array or bit slice: a[1..2]
        ParseSlice ParseString ParseExpr Int Int |

        -- array or bit index: a[3]
        ParseIndex ParseString ParseExpr Int |

        ParseExactly ParseString Literal |

        ParseVariable ParseString Var
    deriving (Show, Eq)


concrete :: ParseExpr -> ParseString
concrete (ParseBinExpr p _ _ _) = p
concrete (ParseUnExpr p _ _) = p
concrete (ParseSlice p _ _ _) = p
concrete (ParseIndex p _ _) = p
concrete (ParseExactly p _) = p
concrete (ParseVariable p _) = p

ast :: ParseExpr -> Expr
ast (ParseBinExpr _ e1 op e2) = (BinExpr (ast e1) op (ast e2))
ast (ParseUnExpr _ op e) = (UnExpr op (ast e))
ast (ParseSlice _ e i1 i2) = (Slice (ast e) i1 i2)
ast (ParseIndex _ e i) = (Index (ast e) i)
ast (ParseExactly _ l) = (Exactly l)
ast (ParseVariable _ v) = (Variable v)

data Expr =
        -- binary arithmatic: +, -, *, \, &,
        BinExpr Expr BinOp Expr |

        -- unary operations: ~, !
        UnExpr UnOp Expr |

        -- array or bit slice: a[1..2]
        Slice Expr Int Int |

        -- array or bit index: a[3]
        Index Expr Int |

        Exactly Literal |

        Variable Var
    deriving (Show, Eq)
