import AST
import Parser
import ParserBase
import Test.Hspec
import Control.Exception (evaluate)


-- Expect the parse to error
-- import arguments: test name, parser, test string
makeErrorTest :: String -> Parser a -> String -> SpecWith ()
makeErrorTest testname parser input = do
        it testname $
            evaluate (parse parser input) `shouldThrow` anyException

main = hspec $ do
    describe "parses literals" $ do
        it ("parses integer literal") $
            (parse expr "1") `shouldBe` (Exactly (Dec 1))

        it ("parses fixed literal") $
            (parse expr "1.1") `shouldBe` (Exactly (Fixed (1) 1))

        it ("parses binary literal") $
            (parse expr "0b11") `shouldBe` (Exactly (Bin "11"))

        -- all hex characters represented in lowercase
        it ("parses hex literal") $
            (parse expr "0xaA") `shouldBe` (Exactly (Hex "aa"))

    describe "parses variable" $ do
        it ("parses variable") $
            (parse expr "abc_123") `shouldBe` (Variable "abc_123")

        makeErrorTest "fails to parse variable starting with number" expr "1abc12"

    describe "parses parentheses" $ do
        it "parses parenthesis" $
            (parse expr "(1)") `shouldBe` (Exactly (Dec 1))

        it "parses nested parenthesis" $
            (parse expr "((1))") `shouldBe` (Exactly (Dec 1))
