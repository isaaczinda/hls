import AST
import Typecheck
import Test.Hspec
import Control.Exception (evaluate)




main = hspec $ do
    -- Test to make sure parse errors are correct
    describe "typechecks literals correctly" $ do
        it ("interprets 0 as UInt1") $
            typecheck (Exactly (Dec 0)) `shouldBe` Just (UIntType 1)

        it ("interprets 1 as UInt1") $
            typecheck (Exactly (Dec 1)) `shouldBe` Just (UIntType 1)

        it ("interprets -1 as Int1") $
            typecheck (Exactly (Dec (-1))) `shouldBe` Just (IntType 1)

        it ("interprets -2 as Int2") $
            typecheck (Exactly (Dec (-2))) `shouldBe` Just (IntType 2)
        



            --
            --
            -- typecheck (Exactly (Dec a))
            --     | a >= 0    = Just (UIntType (unsignedBits a))
            --     | otherwise = Just (IntType (signedBits a))
            --
            -- typecheck (Exactly (Fixed a b)) = Just (FixedType (signedBits a) (unsignedBits b))
            --
            -- typecheck (Exactly (Bin a)) = Just (BitsType (length a))
            --
            -- typecheck (Exactly (Hex a)) = Just (BitsType ((length a) * 4))
