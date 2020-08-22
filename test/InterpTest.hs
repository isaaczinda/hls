-- import AST
-- import Parser

import Test.Hspec
import Control.Exception (evaluate)
import Interp
import AST
import Misc

-- make an expression of any type
exprOfType :: Type -> TExpr
exprOfType ty =
        (Cast ty ty e)
    where
        bits = repeatChar '0' (bitsInType ty)
        e = (Exactly (BitsType (length bits)) (Bin bits))


main = hspec $ do
    describe "interprets literals" $ do
        it "interprets hex literals" $
            interpExpr (Exactly (BitsType 4) (Hex "a")) emptyFrame `shouldBe` "1010"
        it "interprets binary literals" $
            interpExpr (Exactly (BitsType 4) (Bin "1010")) emptyFrame `shouldBe` "1010"
        it "interprets decimal literals" $
            interpExpr (Exactly (UIntType 4) (Dec 10)) emptyFrame `shouldBe` "1010"
        it "interprets fixed literals" $
            interpExpr (Exactly (FixedType 2 1) (Fixed "1.5")) emptyFrame `shouldBe` "011"
        

    --
    -- describe "interprets explicit cast" $ do
    --     --
    --     it "casts IntType to BitsType" $ do
    --         interpExpr ()
