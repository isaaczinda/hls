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
            (parse expr "1.1") `shouldBe` (Exactly ((1, 1), (1, 3)) (Fixed "1.1"))

        it ("parses binary literal") $
            (parse expr "0b11") `shouldBe` (Exactly ((1, 1), (1, 4)) (Bin "11"))

        -- all hex characters represented in lowercase
        it ("parses hex literal") $
            (parse expr "0xaA") `shouldBe` (Exactly ((1, 1), (1, 4)) (Hex "aa"))

        it ("parses bool literals") $ do
            (parse expr "true") `shouldBe` (Exactly ((1, 1), (1, 4)) (Bool True))
            (parse expr "false") `shouldBe` (Exactly ((1, 1), (1, 5)) (Bool False))


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

        it "parses cast to Fixed-1.2" $
            (parse expr "(Fixed-1.2) 1") `shouldBe` (Cast ((1, 1), (1, 13)) (FixedType (-1) 2) (Exactly ((1, 13), (1, 13)) (Dec 1)))

        makeErrorTest "fails to parse invalid (Fixed-1.1) cast" expr "(Fixed-1.1) 0b1"

    describe "parses lists" $ do
        it "parses {1, 2}" $
            (parse expr "{1, 2}") `shouldBe` (List ((1, 1), (1, 6))  [Exactly ((1, 2), (1, 2)) (Dec 1), Exactly ((1, 5), (1, 5)) (Dec 2)])

        it "pares {1}" $
            (parse expr "{1}") `shouldBe` (List ((1, 1), (1, 3))  [Exactly ((1, 2), (1, 2)) (Dec 1)])

        it "parses {}" $
            (parse expr "{}") `shouldBe` (List ((1, 1), (1, 2)) [])

    describe "pareses list index" $ do
        it "pareses var[1+1]" $
            let
                firstOne = (Exactly ((1,5), (1,5)) (Dec 1))
                secondOne = (Exactly ((1,7), (1,7)) (Dec 1))
                varVariable = (Variable ((1,1), (1,3)) "var")
                addExpr = (BinExpr ((1,5), (1,7)) firstOne PlusOp secondOne)
            in
                (parse expr "var[1+1]") `shouldBe` (Index ((1, 1), (1, 8)) varVariable addExpr)

    describe "pareses list slice" $ do
        it "parses 0b010[1..2]" $
            let
                binaryLit = (Exactly ((1, 1), (1, 5)) (Bin "010"))
                firstIndex = (Exactly ((1, 7), (1, 7)) (Dec 1))
                secondIndex = (Exactly ((1, 10), (1, 10)) (Dec 2))
            in
                (parse expr "0b010[1..2]") `shouldBe` (Slice ((1, 1), (1, 11)) binaryLit firstIndex secondIndex)
