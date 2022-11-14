import ParsePython
import Test.Hspec
import Data.Either

main :: IO ()
main = hspec $ describe "Python parser tests" $ do
    it "empty input" $ do
        parsePython "" `shouldBe` Right (Start [])

    it "space input" $ do
        parsePython "   \n\n\t\t\n\n" `shouldBe` Right (Start [])

    it "statements on seperate lines" $ do
        parsePython "1 + 2\n2 + 9" `shouldBe` Right (Start [Arith '+' (Num "1") (Num "2"), Arith '+' (Num "2") (Num "9")])

    it "statements separated by whitespace" $ do
        parsePython "f    \n\n\n\t  \t\t\ng = 10" `shouldBe` Right (Start [Var "f", Assign "=" (Var "g") (Num "10")])

    it "statement preceded by whitespace" $ do
        parsePython "\n\n\t\t\t\n(10 * 10)" `shouldBe` Right (Start [Arith '*' (Num "10") (Num "10")])

    it "statement followed by whitespace" $ do
        parsePython "10     \n\n\t\n\n\n" `shouldBe` Right (Start [Num "10"])

    describe "arithmetic" $ do
        it "simple operation" $ do
            parsePython "18 + 987" `shouldBe` Right (Start [Arith '+' (Num "18") (Num "987")])

        it "three element operation" $ do
            parsePython "10 + 20 * 30" `shouldBe` Right (Start [Arith '+' (Num "10") (Arith '*' (Num "20") (Num "30"))])

        it "four element operation" $ do
            parsePython "10 + 20 * 30 / 100" `shouldBe` Right (Start [Arith '+' (Num "10") (Arith '*' (Num "20") (Arith '/' (Num "30") (Num "100")))])

        it "left parens" $ do
            parsePython "(1 + 2) + 3" `shouldBe` Right (Start [Arith '+' (Arith '+' (Num "1") (Num "2")) (Num "3")])

        it "right parens" $ do
            parsePython "10 + (20 * 30)" `shouldBe` Right (Start [Arith '+' (Num "10") (Arith '*' (Num "20") (Num "30"))])

        it "pointless parens" $ do
            parsePython "(10 % 3)" `shouldBe` Right (Start [Arith '%' (Num "10") (Num "3")])

        it "sets of parens" $ do
            parsePython "((98.7 * -90) + (0.00009 - -89))" `shouldBe` Right (Start [Arith '+' (Arith '*' (Num "98.7") (Num "-90")) (Arith '-' (Num "0.00009") (Num "-89"))])

        it "operation containing a variable" $ do
            parsePython "x * 2" `shouldBe` Right (Start [Arith '*' (Var "x") ( Num "2")])

        it "bigger operation with variable" $ do
            parsePython "((1 + x) + 3)" `shouldBe` Right (Start [Arith '+' (Arith '+' (Num "1") (Var "x")) (Num "3")])

        it "operation with only variables" $ do
            parsePython "x * y + z" `shouldBe` Right (Start [Arith '*' (Var "x") (Arith '+'  (Var "y") ( Var "z"))])

        it "no space between operands" $ do
            parsePython "x+2" `shouldBe` Right (Start [Arith '+' (Var "x") (Num "2")])

        it "extra space between operands" $ do
            parsePython "2    +    lambda" `shouldBe` Right (Start [Arith '+' (Num "2") (Var "lambda")])

        it "extra space between parens" $ do
            parsePython "(   xyz  *  abc  + jkl  )" `shouldBe` Right (Start [Arith '*' (Var "xyz") (Arith '+' (Var "abc") (Var "jkl"))])

    describe "assignment" $ do
        it "variable declaration" $ do
            parsePython "myVar" `shouldBe` Right (Start [Var "myVar"])

        it "variables contain can contain numbers" $ do
            parsePython "myVar8plus500" `shouldBe` Right (Start [Var "myVar8plus500"])

        it "_ is a valid variable name" $ do
            parsePython "_" `shouldBe` Right (Start [Var "_"])

        it "variable names may not be reserved words" $ do
            parsePython "if" `shouldSatisfy` isLeft

        it "basic assignment" $ do
            parsePython "foo = 10.7" `shouldBe` Right (Start [Assign "=" (Var "foo") (Num "10.7")])

        it "assignment with complex arithmetic expression" $ do
            parsePython "snake_var = ((90 - 90) % 7)" `shouldBe` Right (Start [Assign "=" (Var "snake_var") (Arith '%' (Arith '-' (Num "90") (Num "90")) (Num "7"))])

        it "plus equals" $ do
            parsePython "x += 7" `shouldBe` Right (Start [Assign "+=" (Var "x") (Num "7")])

        it "variable set to variable" $ do
            parsePython "y = z" `shouldBe` Right (Start [Assign "=" (Var "y") (Var "z")])

        it "variable set to complex expression containing variables" $ do
            parsePython "lambda *= x * 6 % z" `shouldBe` Right (Start [Assign "*=" (Var "lambda") (Arith '*' (Var "x") (Arith '%' (Num "6") (Var "z")))])

    describe "conditional" $ do
        it "boolean literal" $ do
            parsePython "True" `shouldBe` Right (Start [Bool "True"])

        it "boolean variable" $ do
            parsePython "x" `shouldBe` Right (Start [Var "x"])

        it "variable and boolean" $ do
            parsePython "x or False" `shouldBe` Right (Start [Cond "or" (Var "x") (Bool "False")])

        it "comparison with variables" $ do
            parsePython "x > y" `shouldBe` Right (Start [Cond ">" (Var "x") (Var "y")])

        it "comparison with and operator" $ do
            parsePython "foobar and 9" `shouldBe` Right (Start [Cond "and" (Var "foobar") (Num "9")])

        it "expression with right parens" $ do
            parsePython "-89 != (x * 78)" `shouldBe` Right (Start [Cond "!=" (Num "-89") (Arith '*' (Var "x") (Num "78"))])

        it "arithmetic expression with left parens" $ do
            parsePython "(10 - 8) <= x * 7" `shouldBe` Right (Start [Cond "<=" (Arith '-' (Num "10") (Num "8")) (Arith '*' (Var "x") (Num "7"))])

    describe "if statements" $ do
        it "simple if" $ do
            parsePython "if x > 10:\n    newVar" `shouldBe` Right (Start [If (Cond ">" (Var "x") (Num "10")) [Var "newVar"]])

        it "if with multi line body" $ do
            parsePython "if False:\n    x\n    10" `shouldBe` Right (Start [If (Bool "False") [Var "x", Num "10"]])

        it "if with condition with parens" $ do
            parsePython "if (x != y):\n    x" `shouldBe` Right (Start [If (Cond "!=" (Var "x") (Var "y")) [Var "x"]])

    describe "errors" $ do
        it "needlessly indented statement" $ do
            parsePython " 1 + 2" `shouldSatisfy` isLeft

        it "malformed parens fails" $ do
            parsePython "(x + y) + y)" `shouldSatisfy` isLeft

        it "variable beginning with a number is invalid" $ do
            parsePython "89badvar" `shouldSatisfy` isLeft

