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
