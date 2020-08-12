module AST (ParseString, module AST) where

import Data.Map (Map, lookup, insert, member)
import ParserBase (ParseString)

data Type =
        BoolType |
        UIntType Int |
        IntType Int | --
        FixedType Int Int | -- integer bits, decimal bits
        BitsType Int |
        ListType Type Int |
        EmptyListType -- this is special, it can be converted into any other type !
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


data UnOp = BitNotOp | NotOp | NegOp
    deriving (Eq)

instance Show UnOp where
    show t = case t of
            BitNotOp -> "~"
            NotOp    -> "!"
            NegOp    -> "-"

data Expr a =
        BinExpr a (Expr a) BinOp (Expr a) | -- binary arithmatic
        UnExpr a UnOp (Expr a) | -- unary operations
        Slice a (Expr a) (Expr a) (Expr a) | -- array or bit slice: a[1..2]
        Index a (Expr a) (Expr a) | -- array or bit index: a[3]
        Exactly a Literal |
        Variable a Var |
        Cast a Type (Expr a) |
        List a [(Expr a)]
    deriving (Show, Eq)

type Block a = [Statement a]

data Safety = Safe | Unsafe
    deriving (Show, Eq)

data Statement a =
        If a (Expr a) (Block a) (Maybe (Block a)) |
        -- set variable, check variable bounds, increment variable,
        For a (Statement a) (Expr a) (Statement a) (Block a) |
        -- true -> safe, var, expr
        Assign a Var (Expr a) |
        Declare a Safety Type Var (Expr a)
    deriving (Show, Eq)

-- specific instances of the AST which save the ParseString (hence the 'P')
type PExpr = Expr ParseString
type PStatement = Statement ParseString
type PBlock = Block ParseString


data Frame a =
        Local (Map String a) (Frame a) |
        Global (Map String a)
    deriving (Show, Eq)

-- creates a new variable in the outermost frame
newVar :: Frame a -> String -> a -> Maybe (Frame a)
newVar frame name value =
    case frame of
        (Local m rest) ->
            if (member name m)
                then Nothing
                else Just (Local (insert name value m) rest)
        (Global m)     ->
            if (member name m)
                then Nothing
                else Just (Global (insert name value m))

getVar :: Frame a -> String -> Maybe a
getVar frame name =
    case frame of
        (Local m frame') ->
            case Data.Map.lookup name m of
                Just val -> Just val
                Nothing  -> getVar frame' name
        (Global m)       -> Data.Map.lookup name m


{-
b is a type of kind * -> * (the type takes 1 argument) in the HasExtra typeclass
a is a type of kind *
-}
class HasExtra b where
    getExtra :: b a -> a
    setExtra :: b a -> a -> b a

instance HasExtra Expr where
    getExtra e =
        case e of
                (BinExpr s _ _ _) -> s
                (UnExpr s _ _)    -> s
                (Slice s _ _ _)   -> s
                (Index s _ _)     -> s
                (Exactly s _)     -> s
                (Variable s _)    -> s
                (Cast s _ _)      -> s
                (List s _)        -> s
    setExtra e s =
        case e of
                (BinExpr _ a b c) -> (BinExpr s a b c)
                (UnExpr _ a b)    -> (UnExpr s a b)
                (Slice _ a b c)   -> (Slice s a b c)
                (Index _ a b)     -> (Index s a b)
                (Exactly _ a)     -> (Exactly s a)
                (Variable _ a)    -> (Variable s a)
                (Cast _ a b)      -> (Cast s a b)
                (List _ a)        -> (List s a)

instance HasExtra Statement where
    getExtra e =
        case e of
                (If s _ _ _)        -> s
                (For s _ _ _ _)     -> s
                (Assign s _ _)      -> s
                (Declare s _ _ _ _) -> s
    setExtra e s =
        case e of
                (If _ a b c)        -> (If s a b c)
                (For _ a b c d)     -> (For s a b c d)
                (Assign _ a b)      -> (Assign s a b)
                (Declare _ a b c d) -> (Declare s a b c d)

{-
Calculate whether or not the value of an expression is immediately calculable.
The only criteria for this is that it depends on no variables
-}
isImmdiate :: Expr a -> Bool
isImmdiate e =
    case e of
            (BinExpr _ a _ b) -> (isImmdiate a) && (isImmdiate b)
            (UnExpr _ _ a)    -> isImmdiate a
            (Slice _ a b c)   -> (isImmdiate a) && (isImmdiate b) && (isImmdiate c)
            (Index _ a b)     -> (isImmdiate a) && (isImmdiate b)
            (Exactly _ _)     -> True
            (Variable _ a)    -> False
            (Cast _ _ a)      -> isImmdiate a
            (List _ a)        ->
                let
                    comb = \b e -> b && (isImmdiate e)
                in
                    foldl comb True a

-- combines two parse strings so that the area between their extreme bounds
-- is the new parse string
combParseStrings :: ParseString -> ParseString -> ParseString
combParseStrings (s1, e1) (s2, e2) = (s, e)
    where
        s = if s1 < s2 then s1 else s2
        e = if e1 > e2 then e1 else e2
