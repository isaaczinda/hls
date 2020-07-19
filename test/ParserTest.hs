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


-- (char c) matches the char c
char :: Char -> Parser Char
char c = (get <=> \x -> x == c) <??> ("expected " ++ [c])

-- match the string exactly!
string :: String -> Parser String
string "" = return ""
string s@(h:t) = ((char h) <:> (string t))


main = hspec $ do
    -- Test to make sure parse errors are correct
    describe "gives correct error messages" $ do
        it ("no error message for simple parse") $
            (parseForError (char 'a') "a") `shouldBe` Nothing

        it ("<??> replaces original message") $ do
            -- these should be identical
            (parseForError (char 'a') "b") `shouldBe` (Just ("expected a", (1, 1)))
            (parseForError (string "a") "b") `shouldBe` (Just ("expected a", (1, 1)))

        it ("<??> shadows all child error messages") $
            (parseForError ((string "abc") <??> "expected abc") "abd") `shouldBe` (Just ("expected abc", (1, 1)))

        it ("<???> does not shadow child error messages") $
            (parseForError ((string "abc") <???> "expected abc") "abd") `shouldBe` (Just ("expected c", (1, 3)))

        it ("<???> sets error message when there is no parse") $
            (parseForError ((string "abc") <???> "expected abc") "bbb") `shouldBe` (Just ("expected abc", (1, 1)))

        it ("<???> sets error message when there is no parse") $
            (parseForError ((string "abc") <???> "expected abc") "bbb") `shouldBe` (Just ("expected abc", (1, 1)))

        it ("<|> displays deeper error") $
            (parseForError ((string "aaa") <|> (string "abc")) "abb") `shouldBe` (Just ("expected c", (1, 3)))


    describe "parses literals" $ do
        it ("parses integer literal") $
            (parse expr "1") `shouldBe` (Exactly ((1, 1), (1, 1)) (Dec 1))

        it ("parses fixed literal") $
            (parse expr "1.1") `shouldBe` (Exactly ((1, 1), (1, 3)) (Fixed (1) 1))

        it ("parses binary literal") $
            (parse expr "0b11") `shouldBe` (Exactly ((1, 1), (1, 4)) (Bin "11"))

        -- all hex characters represented in lowercase
        it ("parses hex literal") $
            (parse expr "0xaA") `shouldBe` (Exactly ((1, 1), (1, 4)) (Hex "aa"))

    describe "parses variable" $ do
        it ("parses variable") $
            (parse expr "abc_123") `shouldBe` (Variable ((1, 1), (1, 7)) "abc_123")

        makeErrorTest "fails to parse variable starting with number" expr "1abc12"

    describe "parses parentheses" $ do
        it "parses parenthesis" $
            (parse expr "(1)") `shouldBe` (Exactly ((1, 1), (1, 3)) (Dec 1))

        it "parses nested parenthesis" $
            (parse expr "((1))") `shouldBe` (Exactly ((1, 1), (1, 5)) (Dec 1))

    describe "parses explicit casting" $ do
        it "parses cast to Fixed12.4" $
            (parse expr "(Fixed12.4) 1") `shouldBe` (Cast ((1, 1), (1, 13)) (FixedType 12 4) (Exactly ((1, 13), (1, 13)) (Dec 1)))
