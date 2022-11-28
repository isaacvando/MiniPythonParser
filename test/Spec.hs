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

    it "comment on its own line" $ do
        parsePython "#comment" `shouldBe` Right (Start [])

    it "multiple comments" $ do
        parsePython "#first comment\n\n #   second comment" `shouldBe` Right (Start [])

    it "inline comment after arithmetic" $ do
        parsePython "(10 * 87) - 6 # cool math" `shouldBe` Right (Start [Arith '-' (Arith '*' (Num "10") (Num "87")) (Num "6")])

    it "inline comments after if statement" $ do
        parsePython "if x:   # sick condition\n    foo # that's a pretty sweet variable declaration" `shouldBe` Right (Start [IfStatement [If (Var "x") [Var "foo"]]])

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
            parsePython "if x > 10:\n    newVar" `shouldBe` Right (Start [IfStatement [If (Cond ">" (Var "x") (Num "10")) [Var "newVar"]]])

        it "if with variable condition" $ do
            parsePython "if x:\n    x" `shouldBe` Right (Start [IfStatement [If (Var "x") [Var "x"]]])

        it "if with arithmetic condition" $ do
            parsePython "if 10 * 10:\n    f" `shouldBe` Right (Start [IfStatement [If (Arith '*' (Num "10") (Num "10")) [Var "f"]]])

        it "if with multi line body" $ do
            parsePython "if False:\n    x\n    10" `shouldBe` Right (Start [IfStatement [If (Bool "False") [Var "x", Num "10"]]])

        it "if with condition with parens" $ do
            parsePython "if (x != y):\n    x" `shouldBe` Right (Start [IfStatement [If (Cond "!=" (Var "x") (Var "y")) [Var "x"]]])

        it "if with else" $ do
            parsePython "if False:\n    foo\nelse:\n    bar" `shouldBe` Right (Start [IfStatement [If (Bool "False") [Var "foo"], Else [Var "bar"]]])

        it "if with elif" $ do
            parsePython "if True:\n    foo\nelif False:\n    bar" `shouldBe` Right (Start [IfStatement [If (Bool "True") [Var "foo"], Elif (Bool "False") [Var "bar"]]])
        
        it "two elifs and else" $ do
            parsePython "if True:\n    foo\nelif True:\n    foo\nelif False:\n    foo\nelse:\n    x + 2" `shouldBe`
                Right (Start [IfStatement [If (Bool "True") [Var "foo"], Elif (Bool "True") [Var "foo"], Elif (Bool "False") [Var "foo"], Else [Arith '+' (Var "x") (Num "2")]]])

        it "extra spaces" $ do
            parsePython "if    x     :   \n    foo \n    x" `shouldBe` Right (Start [IfStatement [If (Var "x") [Var "foo", Var "x"]]])

        it "nested if" $ do
            parsePython "if True:\n    if False:\n        g" `shouldBe` Right (Start [IfStatement [If (Bool "True") [IfStatement [If (Bool "False") [Var "g"]]]]])

        it "nested if with other statement" $ do
            parsePython "if x:\n    foo\n    if True:\n        bar" `shouldBe` Right (Start [IfStatement [If (Var "x") [Var "foo", IfStatement [If (Bool "True") [Var "bar"]]]]])

        it "if containing if else chain" $ do
            parsePython "if x:\n    if True:\n        bar\n    else:\n        baz" `shouldBe` Right (Start [IfStatement [If (Var "x") [IfStatement [If (Bool "True") [Var "bar"], Else [Var "baz"]]]]])

        it "if containing whitespace" $ do
            parsePython "if x:\n\n    foo" `shouldBe` Right (Start [IfStatement [If (Var "x") [Var "foo"]]])

        it "if contained spaced ifs" $ do
            parsePython "if x:\n    if y:\n        foo\n    if z:\n        bar" `shouldBe` 
                Right (Start [IfStatement [If (Var "x") [IfStatement [If (Var "y") [Var "foo"]], IfStatement [If (Var "z") [Var "bar"]]]]])

    describe "loops" $ do
        it "simple for" $ do
            parsePython "for x in xs:\n    frobble" `shouldBe` Right (Start [For (Var "x") (Var "xs") [Var "frobble"]])

        it "for with multiple lines" $ do
            parsePython "for foo in frobble:\n    10 + x\n    frobble2" `shouldBe` Right (Start [For (Var "foo") (Var "frobble") [Arith '+' (Num "10") (Var "x"), Var "frobble2"]])

        it "nested for loops" $ do
            parsePython "for xs in xss:\n    for x in xs:\n        foo\n    bar" `shouldBe`
                Right (Start [For (Var "xs") (Var "xss") [For (Var "x") (Var "xs") [Var "foo"], Var "bar"]])


    describe "errors" $ do
        it "needlessly indented statement" $ do
            parsePython " 1 + 2" `shouldSatisfy` isLeft

        it "malformed parens fails" $ do
            parsePython "(x + y) + y)" `shouldSatisfy` isLeft

        it "variable beginning with a number is invalid" $ do
            parsePython "89badvar" `shouldSatisfy` isLeft

        it "only elif" $ do
            parsePython "elif False:\n    foo" `shouldSatisfy` isLeft

        it "only else" $ do
            parsePython "else:\n    frobble" `shouldSatisfy` isLeft

