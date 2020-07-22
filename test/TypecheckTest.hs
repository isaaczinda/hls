import AST
import Typecheck
import Test.Hspec
import Control.Exception (evaluate)
import ParserBase (ParseString)

-- expectTypecheckError :: Expr ->

-- returns True if the type was Err, False if the type was Val
shouldBeErr :: ValOrErr Type -> Expectation
shouldBeErr x =
        (isErr x) `shouldBe` True
    where
        isErr y =
            case y of
                (Val _) -> False
                (Err _) -> True

-- allows function to be applied to something n times
fpow n f x = iterate f x !! n

tmp :: ParseString
tmp = ((0, 0), (0, 0))

uint1 :: Expr
uint1 = Exactly tmp (Dec 1)

int1 :: Expr
int1 = Exactly tmp (Dec (-1))

intn :: Int -> Expr
intn size = Exactly tmp (Dec num)
    -- size - 1 because MSB is taken up with sign
    where num = -(2 ^ (size - 1))


fixed1_2 :: Expr
fixed1_2 = Exactly tmp (Fixed "0.25")

bits1 :: Expr
bits1 = Exactly tmp (Bin "1")


bitsn :: Int -> Expr
bitsn size = Exactly tmp (Bin str)
    where
        str = fpow size (\x -> ('1':x)) ""

main = hspec $ do
    describe "typechecks literals correctly" $ do
        it ("0 is UInt1") $
            (typecheck (Exactly tmp (Dec 0)) "") `shouldBe` Val (UIntType 1)

        it ("1 is UInt1") $
            typecheck (Exactly tmp (Dec 1)) "" `shouldBe` Val (UIntType 1)

        it ("-1 is Int1") $
            typecheck (Exactly tmp (Dec (-1))) "" `shouldBe` Val (IntType 1)

        it ("-2 is Int2") $
            typecheck (Exactly tmp (Dec (-2))) "" `shouldBe` Val (IntType 2)

        it ("-2 is Int2") $
            typecheck (Exactly tmp (Dec (-2))) "" `shouldBe` Val (IntType 2)

        it (".25 is Fixed0.2") $
            typecheck (Exactly tmp (Fixed "0.25")) "" `shouldBe` Val (FixedType 0 2)

        

    describe "typechecks addition correctly" $ do
        it ("UInt1 +/- UInt1 is UInt2") $ do
            typecheck (BinExpr tmp uint1 PlusOp uint1) "" `shouldBe` Val (UIntType 2)
            typecheck (BinExpr tmp uint1 MinusOp uint1) "" `shouldBe` Val (UIntType 2)


        it ("UInt1 +/- Int1 is Int3") $ do
            typecheck (BinExpr tmp uint1 PlusOp int1) "" `shouldBe` Val (IntType 3)
            typecheck (BinExpr tmp uint1 MinusOp int1) "" `shouldBe` Val (IntType 3)


        it ("Fixed1.2 +/- Int1 is Fixed2.2") $ do
            typecheck (BinExpr tmp fixed1_2 PlusOp int1) "" `shouldBe` Val (FixedType 2 2)
            typecheck (BinExpr tmp fixed1_2 MinusOp int1) "" `shouldBe` Val (FixedType 2 2)


        it ("Bits1 +/- Int1 fails") $ do
            shouldBeErr (typecheck (BinExpr tmp bits1 PlusOp int1) "")
            shouldBeErr (typecheck (BinExpr tmp bits1 MinusOp int1) "")


    describe "typechecks multiplication correctly" $ do
        it ("UInt1 * UInt1 is UInt2") $
            typecheck (BinExpr tmp uint1 TimesOp uint1) "" `shouldBe` Val (UIntType 2)

        it ("Int4 * Int4 is Int8") $
            typecheck (BinExpr tmp (intn 4) TimesOp (intn 4)) "" `shouldBe` Val (IntType 8)

        it ("Fixed1.2 * Fixed1.2 is Fixed2.4") $
            typecheck (BinExpr tmp fixed1_2 TimesOp fixed1_2) "" `shouldBe` Val (FixedType 2 4)

    describe "typechecks bitwise operations correctly" $ do
        it ("Int [op] Bits fails") $ do
            shouldBeErr (typecheck (BinExpr tmp bits1 BitAndOp int1) "")
            shouldBeErr (typecheck (BinExpr tmp bits1 BitOrOp int1) "")
            shouldBeErr (typecheck (BinExpr tmp bits1 BitXOrOp int1) "")

        it ("BitsX [op] BitsY fails") $ do
            shouldBeErr (typecheck (BinExpr tmp (bitsn 1) BitAndOp (bitsn 4)) "")
            shouldBeErr (typecheck (BinExpr tmp (bitsn 2) BitOrOp (bitsn 3)) "")
            shouldBeErr (typecheck (BinExpr tmp (bitsn 2) BitXOrOp (bitsn 1)) "")

        it ("Bits3 [op] Bits3 is Bits3") $ do
            typecheck (BinExpr tmp (bitsn 3) BitAndOp (bitsn 3)) "" `shouldBe` Val (BitsType 3)
            typecheck (BinExpr tmp (bitsn 3) BitOrOp (bitsn 3)) "" `shouldBe` Val (BitsType 3)
            typecheck (BinExpr tmp (bitsn 3) BitXOrOp (bitsn 3)) "" `shouldBe` Val (BitsType 3)

    -- describe ""
