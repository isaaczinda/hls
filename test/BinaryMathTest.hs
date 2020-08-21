import BinaryMath
import Test.Hspec
import Control.Exception (evaluate)

main = hspec $ do
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
