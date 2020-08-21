import AST
import TypecheckBase
import TypecheckExpr
import TypecheckStatement
import Test.Hspec
import Control.Exception (evaluate)
import ParserBase (ParseString)

import Data.Map (Map, empty, insert, fromList)

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

uint1 :: PExpr
uint1 = Exactly tmp (Dec 1)

int1 :: PExpr
int1 = Exactly tmp (Dec (-1))

intn :: Int -> PExpr
intn size = Exactly tmp (Dec num)
    -- size - 1 because MSB is taken up with sign
    where num = -(2 ^ (size - 1))

fixed1_2 :: PExpr
fixed1_2 = Exactly tmp (Fixed "0.75")

fixedn :: Int -> Int -> PExpr
fixedn ibits fbits = (Cast tmp (FixedType ibits fbits) (bitsn (ibits + fbits)))

bits1 :: PExpr
bits1 = Exactly tmp (Bin "1")

bitsn :: Int -> PExpr
bitsn size = Exactly tmp (Bin str)
    where
        str = fpow size (\x -> ('1':x)) ""

bool :: PExpr
bool = Exactly tmp (Bool True)

listn :: PExpr -> Int -> PExpr
listn e n = List tmp (fpow n (e:) [])


-- helper to typecheck expressions
checkExpr :: PExpr -> ValOrErr Type
checkExpr e =
    do
        e' <- typecheckExpr e ((Global empty), "")
        return (getExtra e')


checkStatement :: PStatement -> TypeEnv -> (TypeEnv, [String])
checkStatement statement env =
        case retVal of
            Check statement' -> (env', [])
            Errs errs -> (env', errs)
    where
        (env', retVal) = typecheckStatement statement env

main = hspec $ do
    describe "typechecks literals correctly" $ do
        it ("0 is UInt1") $
            checkExpr (Exactly tmp (Dec 0)) `shouldBe` Val (UIntType 1)

        it ("1 is UInt1") $
            checkExpr (Exactly tmp (Dec 1)) `shouldBe` Val (UIntType 1)

        it ("-1 is Int1") $
            checkExpr (Exactly tmp (Dec (-1))) `shouldBe` Val (IntType 1)

        it ("-2 is Int2") $
            checkExpr (Exactly tmp (Dec (-2))) `shouldBe` Val (IntType 2)

        it ("-2 is Int2") $
            checkExpr (Exactly tmp (Dec (-2))) `shouldBe` Val (IntType 2)


        it ("typechecks basic fixed literals") $ do
            -- represented by 0.
            checkExpr (Exactly tmp (Fixed "0.0")) `shouldBe` Val (FixedType 1 0)
            -- represented by 0.1
            checkExpr (Exactly tmp (Fixed "0.5")) `shouldBe` Val (FixedType 1 1)
            -- represented by 01.01
            checkExpr (Exactly tmp (Fixed "1.25")) `shouldBe` Val (FixedType 2 2)
            -- represented by 01.
            checkExpr (Exactly tmp (Fixed "1.0")) `shouldBe` Val (FixedType 2 0)
            -- represented by 010.
            checkExpr (Exactly tmp (Fixed "2.0")) `shouldBe` Val (FixedType 3 0)

        it ("typechecks fixed literals with leading zeroes") $ do
            -- represented by .01
            checkExpr (Exactly tmp (Fixed "0.25")) `shouldBe` Val (FixedType 0 2)
            -- represented by ._01
            checkExpr (Exactly tmp (Fixed "0.125")) `shouldBe` Val (FixedType (-1) 3)
            -- represented by .__01
            checkExpr (Exactly tmp (Fixed "0.0625")) `shouldBe` Val (FixedType (-2) 4)

        it ("typechecks fixed literals with imperfect representations") $ do
            {-
            -.125    : 0
            .0625    : 1
            .03125   : 1
            .015625  : 1
            .0078125 : 1

            Summing these numbers yields .1171875
            The maximum error in this representation should be .005
            The is below the maximum error:
                .12 - .1171875 = .0028125 (less than .005)

            So the representation is .__01111
            -}
            checkExpr (Exactly tmp (Fixed "0.12")) `shouldBe` Val (FixedType (-2) 7)

    describe "typechecks addition correctly" $ do
        it ("UInt1 + UInt1 is UInt2") $ do
            checkExpr (BinExpr tmp uint1 PlusOp uint1) `shouldBe` Val (UIntType 2)

        it ("UInt1 - UInt1 is Int2") $ do
            -- can subtract UInt types (they are implicit cast)
            checkExpr (BinExpr tmp uint1 MinusOp uint1)) `shouldBe` Val (IntType 2)

        it ("UInt1 +/- Int1 is Int3") $ do
            checkExpr (BinExpr tmp uint1 PlusOp int1) `shouldBe` Val (IntType 3)
            checkExpr (BinExpr tmp uint1 MinusOp int1) `shouldBe` Val (IntType 3)


        it ("Fixed1.2 +/- Int1 is Fixed2.2") $ do
            checkExpr (BinExpr tmp fixed1_2 PlusOp int1) `shouldBe` Val (FixedType 2 2)
            checkExpr (BinExpr tmp fixed1_2 MinusOp int1) `shouldBe` Val (FixedType 2 2)


        it ("Bits1 +/- Int1 fails") $ do
            shouldBeErr (checkExpr (BinExpr tmp bits1 PlusOp int1))
            shouldBeErr (checkExpr (BinExpr tmp bits1 MinusOp int1))


    describe "typechecks multiplication correctly" $ do
        it ("UInt1 * UInt1 is UInt2") $
            checkExpr (BinExpr tmp uint1 TimesOp uint1) `shouldBe` Val (UIntType 2)

        it ("Int4 * Int4 is Int8") $
            checkExpr (BinExpr tmp (intn 4) TimesOp (intn 4)) `shouldBe` Val (IntType 8)

        it ("Fixed1.2 * Fixed1.2 is Fixed2.4") $
            checkExpr (BinExpr tmp fixed1_2 TimesOp fixed1_2) `shouldBe` Val (FixedType 2 4)

    describe "typecheck division correctly" $ do
        it ("Fixed2.1 / Fixed3.4 is Fixed6.1") $
            checkExpr (BinExpr tmp (fixedn 2 1) DivOp (fixedn 3 4)) `shouldBe` Val (FixedType 6 1)


    describe "typechecks bitwise operations correctly" $ do
        it ("Int [op] Bits fails") $ do
            shouldBeErr (checkExpr (BinExpr tmp bits1 BitAndOp int1))
            shouldBeErr (checkExpr (BinExpr tmp bits1 BitOrOp int1))
            shouldBeErr (checkExpr (BinExpr tmp bits1 BitXOrOp int1))

        it ("BitsX [op] BitsY fails") $ do
            shouldBeErr (checkExpr (BinExpr tmp (bitsn 1) BitAndOp (bitsn 4)))
            shouldBeErr (checkExpr (BinExpr tmp (bitsn 2) BitOrOp (bitsn 3)))
            shouldBeErr (checkExpr (BinExpr tmp (bitsn 2) BitXOrOp (bitsn 1)))

        it ("Bits3 [op] Bits3 is Bits3") $ do
            checkExpr (BinExpr tmp (bitsn 3) BitAndOp (bitsn 3)) `shouldBe` Val (BitsType 3)
            checkExpr (BinExpr tmp (bitsn 3) BitOrOp (bitsn 3)) `shouldBe` Val (BitsType 3)
            checkExpr (BinExpr tmp (bitsn 3) BitXOrOp (bitsn 3)) `shouldBe` Val (BitsType 3)

    describe "typechecks unary negative operator correctly" $ do
        it ("-(UInt1) is Int2") $
            checkExpr (UnExpr tmp NegOp uint1) `shouldBe` Val (IntType 2)

        it ("-(Int1) is Int2") $
            checkExpr (UnExpr tmp NegOp int1) `shouldBe` Val (IntType 2)

        it ("-1.0 is Fixed1.0") $ do
            checkExpr (UnExpr tmp NegOp (Exactly tmp (Fixed "1.0"))) `shouldBe` Val (FixedType 1 0)

        it ("-4.0 is Fixed3.0") $ do
            checkExpr (UnExpr tmp NegOp (Exactly tmp (Fixed "4.0"))) `shouldBe` Val (FixedType 3 0)

        it ("-3.75 is Fixed3.2") $ do
            checkExpr (UnExpr tmp NegOp (Exactly tmp (Fixed "3.75"))) `shouldBe` Val (FixedType 3 2)

        -- because we can't know whether ((Fixed1.0) 0b1) will move out of range
        -- or not, we have to have the result be 1 larger
        it ("-((Fixed1.0) 0b1) is Fixed2.0") $ do
            checkExpr (UnExpr tmp NegOp (Cast tmp (FixedType 1 0) (Exactly tmp (Bin "1")))) `shouldBe` Val (FixedType 2 0)

        it ("cannot use '-' on Bool or Bits") $ do
            shouldBeErr (checkExpr (UnExpr tmp NegOp (Exactly tmp (Bool True))))
            shouldBeErr (checkExpr (UnExpr tmp NegOp bits1))

    describe "typechecks list creation correctly" $ do
        it ("{} is None[0]") $
            checkExpr (listn uint1 0) `shouldBe` Val EmptyListType

        it ("{1} is UInt1[1]") $
            checkExpr (listn uint1 1) `shouldBe` Val (ListType (UIntType 1) 1)

        it ("{1, 1} is UInt1[2]") $
            checkExpr (listn uint1 2) `shouldBe` Val (ListType (UIntType 1) 2)

        it ("{-1, 1} is Int2[2]") $
            checkExpr (List tmp [int1, uint1]) `shouldBe` Val (ListType (IntType 2) 2)

        it ("{Bits4, Bits4} is Bits4[2]") $
            checkExpr (listn (bitsn 4) 2) `shouldBe` Val (ListType (BitsType 4) 2)

        it ("{BitsX, BitsY} does not typecheck") $
            shouldBeErr (checkExpr (List tmp [bitsn 3, bitsn 4]))

    describe "typechecks list indexing correctly" $ do
        it ("{1}[0] is UInt1") $
            let
                list = (listn uint1 1)
                index = (Exactly tmp (Dec 1))
            in
                checkExpr (Index tmp list index) `shouldBe` Val (UIntType 1)

        it ("0b0101[0] is Bits1") $
            checkExpr (Index tmp (Exactly tmp (Bin "0101")) (Exactly tmp (Dec 0))) `shouldBe` Val (BitsType 1)

        it ("{1}[-1] does not typecheck") $
            shouldBeErr (checkExpr (Index tmp (listn uint1 1) (Exactly tmp (Dec (-1)))))

    describe "typechecks &&" $ do
        it "Bool && Bool is Bool" $
            checkExpr (BinExpr tmp bool AndOp bool) `shouldBe` Val BoolType

        it "fails to typecheck Bool && Bits1" $
            shouldBeErr (checkExpr (BinExpr tmp bool AndOp (bitsn 1)))

    describe "typechecks ||" $ do
        it "Bool || Bool is Bool" $
            checkExpr (BinExpr tmp bool OrOp bool) `shouldBe` Val BoolType

        it "fails to typecheck Bool || Bits1" $
            shouldBeErr (checkExpr (BinExpr tmp bool OrOp (bitsn 1)))

    describe "typechecks == and !=" $ do
        it "fails to typecheck Bool == Int1" $
            shouldBeErr (checkExpr (BinExpr tmp bool EqualsOp (intn 1)))

        it "Bits1 == Bool is Bool" $
            shouldBeErr (checkExpr (BinExpr tmp (bitsn 1) EqualsOp bool))

        it "UInt1 == Int15 is Bool" $
            checkExpr (BinExpr tmp (uint1) EqualsOp (intn 15)) `shouldBe` Val BoolType

        it "UInt1 == Fixed12.3 is Bool" $
            checkExpr (BinExpr tmp (uint1) EqualsOp (fixedn 12 3)) `shouldBe` Val BoolType

    describe "typechecks ++" $ do
        it "Bits4 ++ Bits4 is Bits8" $
            checkExpr (BinExpr tmp (bitsn 4) ConcatOp (bitsn 4)) `shouldBe` Val (BitsType 8)

        it "Bits4[2] ++ Bits4[3] is Bits4[5]" $
            checkExpr (BinExpr tmp (listn (bitsn 4) 2) ConcatOp (listn (bitsn 4) 3)) `shouldBe` Val (ListType (BitsType 4) 5)

        it "[] ++ Bits4[3] is Bits4[3]" $ do
            checkExpr (BinExpr tmp (listn (bitsn 4) 0) ConcatOp (listn (bitsn 4) 3)) `shouldBe` Val (ListType (BitsType 4) 3)
            checkExpr (BinExpr tmp (listn (bitsn 4) 3) ConcatOp (listn (bitsn 4) 0)) `shouldBe` Val (ListType (BitsType 4) 3)

        it "[] ++ [] is []" $
            checkExpr (BinExpr tmp (listn (bitsn 4) 0) ConcatOp (listn (bitsn 4) 0)) `shouldBe` Val EmptyListType

    let safeUInt2Env = (Global (fromList [("test", (UIntType 2, Safe))]), "")
    let unsafeUInt2Env = (Global (fromList [("test", (UIntType 2, Unsafe))]), "")
    let emptyEnv = (emptyFrame, "")

    describe "typechecks declare statements" $ do
        let decSafeOverflow = Declare tmp Safe (UIntType 2) "test" (Exactly tmp (Dec 4))
        let decSafeNoOverflow = Declare tmp Safe (UIntType 2) "test" (Exactly tmp (Dec 1))
        let decUnsafeOverflow = Declare tmp Unsafe (UIntType 2) "test" (Exactly tmp (Dec 4))

        it "declare safe variable without overflow" $ do
            let (env, errs) = checkStatement decSafeNoOverflow emptyEnv
            errs `shouldBe` [] -- should be no errors
            env `shouldBe` safeUInt2Env

        it "declare unsafe variable with overflow" $ do
            let (env, errs) = checkStatement decUnsafeOverflow emptyEnv
            (length errs) `shouldBe` 0
            env `shouldBe` unsafeUInt2Env

        it "can't declare safe variable with overflow" $ do
            let (env, errs) = checkStatement decSafeOverflow emptyEnv
            (length errs) `shouldBe` 1
            env `shouldBe` emptyEnv

        it "can't declare variable twice" $ do
            let (env, errs) = checkStatement decSafeNoOverflow safeUInt2Env
            (length errs) `shouldBe` 1
            env `shouldBe` safeUInt2Env

    describe "typechecks assign statements" $ do
        let assign = Assign tmp "test" (Exactly tmp (Dec 2))
        let assignOverflow = Assign tmp "test" (Exactly tmp (Dec 4))

        it "allows assignment" $ do
            let (env, errs) = checkStatement assign safeUInt2Env
            (length errs) `shouldBe` 0
            env `shouldBe` safeUInt2Env

        it "doesn't allow assignment without previous declaration" $ do
            let (env, errs) = checkStatement assign emptyEnv
            (length errs) `shouldBe` 1
            env `shouldBe` emptyEnv

        it "doesn't allow overflow assignment to safe var" $ do
            let (env, errs) = checkStatement assignOverflow safeUInt2Env
            (length errs) `shouldBe` 1
            env `shouldBe` safeUInt2Env

        it "allows overflow assignment to unsafe var" $ do
            let (env, errs) = checkStatement assignOverflow unsafeUInt2Env
            (length errs) `shouldBe` 0
            env `shouldBe` unsafeUInt2Env

    describe "typechecks if statement" $ do
        it "if (true) {}" $ do
            let stat = If tmp (Exactly tmp (Bool True)) [] Nothing
            let (env, err) = checkStatement stat emptyEnv
            (length err) `shouldBe` 0

        it "if (true) {} else {}" $ do
            let stat = If tmp (Exactly tmp (Bool True)) [] (Just [])
            let (env, err) = checkStatement stat emptyEnv
            (length err) `shouldBe` 0

        it "fails to typecheck if(1) {}" $ do
            let stat = If tmp (Exactly tmp (Dec 1)) [] Nothing
            let (env, err) = checkStatement stat emptyEnv
            (length err) `shouldBe` 1


    describe "typechecks for statement" $ do
        let dec = (Declare tmp Unsafe (UIntType 1) "i" (Exactly tmp (Dec 0)))
        let check = (BinExpr tmp (Variable tmp "i") NotEqualsOp (Exactly tmp (Dec 0)))
        let inc = (Assign tmp "i" (BinExpr tmp (Variable tmp "i") PlusOp (Exactly tmp (Dec 1))))

        it "for (UInt1 i = 0; i != 0; i /=/ i + 1) {}" $ do
            let (_, err) = checkStatement (For tmp dec check inc []) emptyEnv
            (length err) `shouldBe` 0

        it "inner block can access outer variables" $ do
            let (_, err) = checkStatement (For tmp dec check inc [inc]) emptyEnv
            (length err) `shouldBe` 0

        it "increment clause must perform assignment" $ do
            let (_, err) = checkStatement (For tmp dec check dec []) emptyEnv
            (length err) `shouldBe` 1
