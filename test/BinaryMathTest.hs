import BinaryMath
import Test.Hspec
import Control.Exception (evaluate)

main = hspec $ do
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

        it "intToBin 1 == '01'" $
            intToBin 1 `shouldBe` "01"

        it "intToBin 3 == '011'" $
            intToBin 3 `shouldBe` "011"

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
