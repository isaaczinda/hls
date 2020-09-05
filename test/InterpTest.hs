-- import AST
-- import Parser

import Test.Hspec
import Control.Exception (evaluate)
import Interp
import AST
import Misc
import Control.Exception (evaluate)
import Frame

shouldBeErr :: a -> Expectation
shouldBeErr val = evaluate (val) `shouldThrow` anyException

-- make an expression of any type
exprOfType :: Type -> String -> TExpr
exprOfType ty bits =
        (Cast ty ty e)
    where
        e = (Exactly (BitsType (length bits)) (Bin bits))

testCast :: String -> Type -> Type -> String
testCast val ty ty' =
    interpExpr (Cast ty' ty' (exprOfType ty val)) emptyFrame


main = hspec $ do
    describe "interprets literals" $ do
        it "interprets hex literals" $
            interpExpr (Exactly (BitsType 4) (Hex "a")) emptyFrame `shouldBe` "1010"
        it "interprets binary literals" $
            interpExpr (Exactly (BitsType 4) (Bin "1010")) emptyFrame `shouldBe` "1010"
        it "interprets decimal literals" $
            interpExpr (Exactly (UIntType 4) (Dec 10)) emptyFrame `shouldBe` "1010"

        describe "interprets fixed literals" $ do
            it "1.5 is 001" $
                interpExpr (Exactly (FixedType 2 1) (Fixed "1.5")) emptyFrame `shouldBe` "011"
            it "-.25 is 1" $
                interpExpr (Exactly (FixedType (-1) 2) (Fixed "-0.25")) emptyFrame `shouldBe` "1"
            it ".25 is 01" $
                interpExpr (Exactly (FixedType (-1) 2) (Fixed "0.25")) emptyFrame `shouldBe` "01"

        it "interprets bool literals" $
            interpExpr (Exactly BoolType (Bool True)) emptyFrame `shouldBe` "1"

    describe "interprets explicit cast" $ do
        describe "casts anything to BitsType" $ do
            it "casts UInt4 to Bits4" $
                testCast "1010" (UIntType 4) (BitsType 4) `shouldBe` "1010"
            it "casts Int4 to Bits4" $
                testCast "1010" (IntType 4) (BitsType 4)`shouldBe` "1010"
            it "casts Fixed2.2 to Bits4" $
                testCast "1010" (FixedType 2 2) (BitsType 4)`shouldBe` "1010"
            it "casts Bool to Bits1" $
                testCast "1" BoolType (BitsType 1)`shouldBe` "1"

        describe "casts BitsType to anything" $ do
            it "casts Bits4 to UInt4" $
                testCast "1010" (BitsType 4) (UIntType 4) `shouldBe` "1010"
            it "casts Bits4 to Int4" $
                testCast "1010" (BitsType 4) (IntType 4) `shouldBe` "1010"
            it "casts Bits4 to Fixed2.2" $
                testCast "1010" (BitsType 4) (FixedType 2 2) `shouldBe` "1010"
            it "casts Bits1 to Bool" $
                testCast "0" (BitsType 1) BoolType `shouldBe` "0"

        describe "casts between numeric types" $ do
            it "casts UInt to UInt" $ do
                testCast "11" (UIntType 2) (UIntType 4) `shouldBe` "0011"
                testCast "11" (UIntType 2) (UIntType 1) `shouldBe` "1"

            it "casts UInt to Int" $ do
                testCast "11" (UIntType 2) (IntType 4) `shouldBe` "0011"
                testCast "111" (UIntType 3) (IntType 2) `shouldBe` "11"

            it "casts UInt to Fixed" $ do
                testCast "11" (UIntType 2) (FixedType 3 2) `shouldBe` "01100"
                testCast "11" (UIntType 2) (FixedType 1 2) `shouldBe` "100"

            it "casts Int to UInt" $ do
                testCast "11" (IntType 2) (UIntType 4) `shouldBe` "0011"
                testCast "1111" (IntType 4) (UIntType 2) `shouldBe` "11"

            it "casts Int to Int" $ do
                testCast "111" (IntType 3) (IntType 2) `shouldBe` "11"
                testCast "111" (IntType 3) (IntType 4) `shouldBe` "1111"

            it "casts Int to Fixed" $ do
                testCast "10" (IntType 2) (FixedType 3 1) `shouldBe` "1100"
                testCast "100" (IntType 3) (FixedType 2 1) `shouldBe` "000"

            it "casts Fixed to UInt" $ do
                -- -3.5
                testCast "1001" (FixedType 3 1) (UIntType 2) `shouldBe` "00"
                testCast "1001" (FixedType 3 1) (UIntType 4) `shouldBe` "0100"

            it "casts Fixed to Int" $ do
                testCast "1101" (FixedType 3 1) (IntType 2) `shouldBe` "10"
                testCast "1101" (FixedType 3 1) (IntType 4) `shouldBe` "1110"

            it "casts Fixed to Fixed" $ do
                -- truncates decimal properly
                testCast "1001" (FixedType 2 2) (FixedType 2 1) `shouldBe` "100"
                -- extends decimal properly
                testCast "1001" (FixedType 2 2) (FixedType 2 3) `shouldBe` "10010"
                -- truncates integer properly
                testCast "1001" (FixedType 2 2) (FixedType 1 2) `shouldBe` "001"
                -- extends integer properly
                testCast "1001" (FixedType 2 2) (FixedType 3 2) `shouldBe` "11001"

        describe "interprests arithmetic" $ do
            let oneuint = (Exactly (UIntType 1) (Dec 1))
            let twouint = (Exactly (UIntType 2) (Dec 2))
            let threeuint = (Exactly (UIntType 2) (Dec 3))

            let noneint = (Exactly (IntType 1) (Dec (-1)))
            let ntwoint = (Exactly (IntType 2) (Dec (-2)))

            let oneint = (Cast (IntType 2) (IntType 2) oneuint)
            let twoint = (Cast (IntType 3) (IntType 3) twouint)
            let threeint = (Cast (IntType 3) (IntType 3) threeuint)

            let pointfive = (Exactly (FixedType 1 1) (Fixed "0.5"))
            let onepointfive = (Exactly (FixedType 2 1) (Fixed "1.5"))
            let npointtwofive = (Exactly (FixedType (-1) 2) (Fixed "-0.25"))


            describe "interprets addition" $ do
                it "2 + 3 == 5" $
                    interpExpr (BinExpr (UIntType 3) twouint PlusOp threeuint) emptyFrame `shouldBe` "101"

                it "(Int) 2 + (Int) 3 == (Int) 5" $
                    interpExpr (BinExpr (IntType 4) twoint PlusOp threeint) emptyFrame `shouldBe` "0101"

                it "-1 + -1 == -2" $
                    interpExpr (BinExpr (IntType 2) noneint PlusOp noneint) emptyFrame `shouldBe` "10"

            describe "interprets multiplication" $ do
                it "-2 * -1 == 2" $
                    interpExpr (BinExpr (IntType 3) ntwoint TimesOp noneint) emptyFrame `shouldBe` "010"
                it "-2 * 3 == -6" $
                    interpExpr (BinExpr (IntType 5) ntwoint TimesOp threeint) emptyFrame `shouldBe` "11010"

                it ".5 * .5 == .25" $
                    interpExpr (BinExpr (FixedType 2 2) pointfive TimesOp pointfive) emptyFrame `shouldBe` "0001"

                it "-.25 * -.25 == .0625" $
                    interpExpr (BinExpr (FixedType (-2) 4) npointtwofive TimesOp npointtwofive) emptyFrame `shouldBe` "01"

                it "test" $
                    interpExpr onepointfive emptyFrame `shouldBe` "011"
                
                -- Fixed2.1 (011) * Fixed-1.2 (1) == Fixed1.3
                it "1.5 * -.25 == -.375" $
                    interpExpr (BinExpr (FixedType 1 3) onepointfive TimesOp npointtwofive) emptyFrame `shouldBe` "1101"
