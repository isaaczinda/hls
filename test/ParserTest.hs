import AST
import Parser
import Test.Hspec
import Control.Exception (evaluate)


-- Expect the parse to error
-- import arguments: test name, parser, test string
makeErrorTest :: String -> Parser a -> String -> SpecWith ()
makeErrorTest testname parser input = do
        it testname $
            evaluate (parse parser input) `shouldThrow` anyException


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

    describe "make sure multi-line parse works" $ do
        it "parses aa\\naa" $ do
            let aa = (string "aa") --> \x s -> return s

            (parse (aa <+-> ws <+> aa) "aa\naa") `shouldBe` (((1, 1), (1, 2)), ((2, 1), (2, 2)))

    describe "parses literals" $ do
        describe ("parses integer literal") $ do
            it "parses 1" $
                (parse expr "1") `shouldBe` (Exactly ((1, 1), (1, 1)) (Dec 1))
            it "parses -1" $
                (parse expr "-1") `shouldBe` (Exactly ((1, 1), (1, 2)) (Dec (-1)))

        describe ("parses fixed literal") $ do
            it "parses 1.1" $
                (parse expr "1.1") `shouldBe` (Exactly ((1, 1), (1, 3)) (Fixed "1.1"))
            it "parses -1.1" $
                (parse expr "-1.1") `shouldBe` (Exactly ((1, 1), (1, 4)) (Fixed "-1.1"))

        it ("parses binary literal") $
            (parse expr "0b11") `shouldBe` (Exactly ((1, 1), (1, 4)) (Bin "11"))

        -- all hex characters represented in lowercase
        it ("parses hex literal") $
            (parse expr "0xaA") `shouldBe` (Exactly ((1, 1), (1, 4)) (Hex "aa"))

        it ("parses bool literals") $ do
            (parse expr "true") `shouldBe` (Exactly ((1, 1), (1, 4)) (Bool True))
            (parse expr "false") `shouldBe` (Exactly ((1, 1), (1, 5)) (Bool False))

    describe "parses types" $ do
        it "parses UInt1" $
            (parse types "UInt1") `shouldBe` (UIntType 1)
        it "parses Int10" $
            (parse types "Int10") `shouldBe` (IntType 10)
        it "parses Fixed-2.4" $
            (parse types "Fixed-2.4") `shouldBe` (FixedType (-2) 4)
        it "parses Bool" $
            (parse types "Bool") `shouldBe` BoolType
        it "parses Bits11" $
            (parse types "Bits11") `shouldBe` (BitsType 11)
        it "parses Bits12[12]" $
            (parse types "Bits12[12]") `shouldBe` (ListType (BitsType 12) 12)
        it "parses Bits4[12][11]" $
            (parse types "Bits4[12][11]") `shouldBe` (ListType ((ListType (BitsType 4) 11)) 12)

    describe "parses variable" $ do
        it ("parses abc_123") $
            (parse var "abc_123") `shouldBe` "abc_123"

        it ("parses Bits") $
            (parse var "Bits") `shouldBe` "Bits"

        makeErrorTest "fails to parse variable which is actually control flow" var "else"

        makeErrorTest "fails to parse variable which is actually a type" var "Bits2"

        makeErrorTest "fails to parse variable starting with number" var "1abc12"

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

    describe "parses declaration" $ do
        it "parses UInt test = 1;" $
            (parse statement "UInt1 test = 1;") `shouldBe` (Declare ((1, 1), (1, 15)) Safe (UIntType 1) "test" (Exactly ((1, 14), (1, 14)) (Dec 1)))
        it "parses UInt test=1   ;" $
            (parse statement "UInt1 test=1   ;") `shouldBe` (Declare ((1, 1), (1, 16)) Safe (UIntType 1) "test" (Exactly ((1, 12), (1, 12)) (Dec 1)))
        it "parses unsafe UInt1 i=1;" $
            (parse statement "unsafe UInt1 i=1;") `shouldBe` (Declare ((1, 1), (1, 17)) Unsafe (UIntType 1) "i" (Exactly ((1, 16), (1, 16)) (Dec 1)))

    describe "parses assignment" $ do
        it "parses i = 1;" $
            (parse statement "i = 1;") `shouldBe` (Assign ((1, 1), (1, 6)) "i" (Exactly ((1, 5), (1, 5)) (Dec 1)))

    describe "parses if-else statement" $ do
        it "parses if(true){UInt1 i=1;}" $ do
            let cond = (Exactly ((1, 4), (1, 7)) (Bool True))
            let ifblock = [(Declare ((1, 10), (1, 19)) Safe (UIntType 1) "i" (Exactly ((1, 18), (1, 18)) (Dec 1)))]

            (parse statement "if(true){UInt1 i=1;}") `shouldBe` (If ((1, 1), (1, 20)) cond ifblock Nothing)

        it "parses if(true){}else{i=1;}" $ do
            let cond = (Exactly ((1, 4), (1, 7)) (Bool True))
            let elseblock = [(Assign ((1, 16), (1, 19)) "i" (Exactly ((1, 18), (1, 18)) (Dec 1)))]

            (parse statement "if(true){}else{i=1;}") `shouldBe` (If ((1, 1), (1, 20)) cond [] (Just elseblock))

    describe "parses for statement" $ do
        it "parses for (i=0; i==2; i=i+1) {\\nn=n*i;\\n}" $ do
            let set = Assign ((1, 6), (1, 9)) "i" (Exactly ((1, 8), (1, 8)) (Dec 0))
            let check = BinExpr ((1, 11), (1, 14)) (Variable ((1, 11), (1, 11)) "i") EqualsOp (Exactly ((1, 14), (1, 14)) (Dec 2))
            let inc = Assign ((1, 17), (1, 21)) "i" (BinExpr ((1, 19), (1, 21)) (Variable ((1, 19), (1, 19)) "i") PlusOp (Exactly ((1, 21), (1, 21)) (Dec 1)))
            let blk = [Assign ((2, 1), (2, 6)) "n" (BinExpr ((2, 3), (2, 5)) (Variable ((2, 3), (2, 3)) "n") TimesOp (Variable ((2, 5), (2, 5)) "i"))]

            (parse statement "for (i=0; i==2; i=i+1) {\nn=n*i;\n}") `shouldBe` (For ((1, 1), (3, 1)) set check inc blk)
