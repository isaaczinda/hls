import BinaryMath
import Test.Hspec
import Control.Exception (evaluate)
import AST

main = hspec $ do
    describe "binToUInt works" $ do
        it "binToUInt 010 == 2" $
            binToUInt "010" `shouldBe` 2

        it "binToUInt 110 == 6" $
            binToUInt "110" `shouldBe` 6

    describe "intBits works" $ do
        it "intBits -4 == 3" $
            intBits (-4) `shouldBe` 3

        it "intBits 1 == 2" $
            intBits 1 `shouldBe` 2

        it "intBits -1 == 1" $
            intBits (-1) `shouldBe` 1

        it "intBits 4 == 4" $
            intBits 4 `shouldBe` 4

        it "intBits 0 == 1" $
            intBits 0 `shouldBe` 1

    describe "uintBits works" $ do
        it "uintBits 2 == 2" $
            uintBits 2 `shouldBe` 2

        it "uintBits 0 == 1" $
            uintBits 0 `shouldBe` 1

    describe "uintToBin works" $ do
        it "uintToBin 0 == '0'" $
            uintToBin 0 `shouldBe` "0"

        it "uintToBin 2 == '10'" $
            uintToBin 2 `shouldBe` "10"

        it "uintToBin 7 == '111'" $
            uintToBin 7 `shouldBe` "111"

    describe "intToBin works" $ do
        it "intToBin 0 == '0'" $
            intToBin 0 `shouldBe` "0"

        it "intToBin -1 == '1'" $
            intToBin (-1) `shouldBe` "1"

        it "intToBin -12 == '10100'" $
            intToBin (-12) `shouldBe` "10100"

        it "intToBin 1 == '01'" $
            intToBin 1 `shouldBe` "01"

        it "intToBin 3 == '011'" $
            intToBin 3 `shouldBe` "011"

    describe "fixedToBin works" $ do
        it "fixedToBin '1' == '01'" $
            fixedToBin "1" `shouldBe` "01"

        it "fixedToBin '-1' == '1'" $
            fixedToBin "-1" `shouldBe` "1"

        it "fixedToBin '-4' == '100'" $
            fixedToBin "-4" `shouldBe` "100"

        it "fixedToBin '-1.5' == '101'" $
            fixedToBin "-1.5" `shouldBe` "101"

        it "fixedToBin '-3.25' == '10011'" $
            fixedToBin "-3.25" `shouldBe` "10011"

        it "fixedToBin '2.75' == '01011'" $
            fixedToBin "2.75" `shouldBe` "01011"

        -- 0 integer bits
        it "fixedToBin '0.25' == '01'" $
            fixedToBin "0.25" `shouldBe` "01"

        -- -1 integer bits
        it "fixedToBin '0.125' == '01'" $
            fixedToBin "0.125" `shouldBe` "01"

    describe "fixedHelper works" $ do
        it "fixedHelper '-3.5' == '1001'" $
            fixedHelper "-3.5" `shouldBe` ("1001", FixedType 3 1)

        it "fixedHelper '3.5' == '0111'" $
            fixedHelper "3.5" `shouldBe` ("0111", FixedType 3 1)

        it "fixedHelper '-0.125' == '1'" $
            fixedHelper "-0.125" `shouldBe` ("1", FixedType (-2) 3)

        it "fixedHelper '0.125' == '01'" $
            fixedHelper "0.125" `shouldBe` ("01", FixedType (-1) 3)

        it "fixedHelper '0' == '0'" $
            fixedHelper "0" `shouldBe` ("0", FixedType 1 0)


    describe "add works" $ do
        it "add '111' '111' == '1110'" $
            add "111" "111" `shouldBe` "1110"
        it "add '000' '000' -- '0000'" $
            add "000" "000" `shouldBe` "0000"
        it "add '100' '001' == '0101'" $
            add "100" "001" `shouldBe` "0101"

    describe "negative works" $ do
        it "negative '111' == '001'" $
            negative "111" `shouldBe` "0001"
        it "negative '100' == '0100'" $
            negative "100" `shouldBe` "0100"

    describe "bitwise not works" $ do
        it "bitNot '111' == '000'" $
            bitNot "111" `shouldBe` "000"

    describe "bitwise and works" $ do
        it "bitAnd '110' '100' == '100'" $
            bitAnd "110" "100" `shouldBe` "100"

    describe "bitwise or works" $ do
        it "bitOr '110' '010' == '110'" $
            bitOr "110" "010" `shouldBe` "110"

    describe "bitwise xor works" $ do
        it "bitXOr '110' '010' == '100'" $
            bitXOr "110" "010" `shouldBe` "100"

    describe "multiply works" $ do
        describe "multiply uint works" $ do
            -- 3 * 4 == 12
            it "11 * 100 == 001100" $
                multiply False "11" "100" `shouldBe` "01100"

            -- 3 * 1 == 3
            it "011 * 001 == 000011" $
                multiply False "011" "001" `shouldBe` "000011"

            -- 0 * 0 == 0
            it "00 * 00 == 0000" $
                multiply True "00" "00" `shouldBe` "0000"

        describe "multiply int works" $ do
            -- -1 * -4 == 4
            it "11 * 100 == 001100" $
                multiply True "11" "100" `shouldBe` "00100"

            -- -1 * -1 == 1
            it "11 * 11 == 0001" $
                multiply True "11" "11" `shouldBe` "0001"

        describe "multiply fixed works" $ do
            it "01.11 * 011. == 00101.01" $
                multiplyFixed (FixedType 2 2) (FixedType 3 0) "0111" "011" `shouldBe` "0010101"

            -- Fixed-1.2 * Fixed-1.2 == Fixed-2.4
            -- -.25 * -.25 == .0625
            it "._1 * ._1 == .__01" $
                multiplyFixed (FixedType (-1) 2) (FixedType (-1) 2) "1" "1" `shouldBe` "01"

            -- Fixed-2.3 * Fixed4.0 == Fixed2.3
            -- -.125 * 4 == -.5
            it ".__1 * 0100. == 11.100" $
                multiplyFixed (FixedType (-2) 3) (FixedType 4 0) "1" "0100" `shouldBe` "11100"
            
            -- Fixed-1.2 * Fixed2.1 == Fixed1.3
            -- -.25 * 1.5 == -.375
            it "._1 * 01.1 == 1.101" $
                multiplyFixed (FixedType (-1) 2) (FixedType 2 1) "1" "011" `shouldBe` "1101"
