import ParsePython
import Test.Hspec
import Data.Either

main :: IO ()
main = hspec $ do
    describe "Python parser tests" $ do
        it "empty input" $ do
            parsePython "" `shouldBe` (Right $ Start []) 
        
        describe "arithmetic" $ do
            it "simple operation" $ do
                parsePython "18 + 987" `shouldBe` (Right $ Start [Arith '+' [Num "18", Num "987"]])

            it "three element operation" $ do
                parsePython "10 + 20 * 30" `shouldBe` (Right $ Start [Arith '+' [Num "10", Arith '*' [Num "20", Num "30"]]])

            it "four element operation" $ do
                parsePython "10 + 20 * 30 / 100" `shouldBe` (Right $ Start [Arith '+' [Num "10", Arith '*' [Num "20", Arith '/' [Num "30", Num "100"]]]])

            it "left parens" $ do
                parsePython "(1 + 2) + 3" `shouldBe` (Right $ Start [Arith '+' [Arith '+' [Num "1", Num "2"], Num "3"]])

            it "right parens" $ do
                parsePython "10 + (20 * 30)" `shouldBe` (Right $ Start [Arith '+' [Num "10", Arith '*' [Num "20", Num "30"]]])

            it "pointless parens" $ do 
                parsePython "(10 % 3)" `shouldBe` (Right $ Start [Arith '%' [Num "10", Num "3"]])

            it "sets of parens" $ do
                parsePython "((98.7 * -90) + (0.00009 - -89))" `shouldBe` (Right $ Start [Arith '+' [Arith '*' [Num "98.7", Num "-90"], Arith '-' [Num "0.00009", Num "-89"]]])

            it "statements on seperate lines" $ do
                parsePython "1 + 2\n2 + 9" `shouldBe` (Right $ Start [Arith '+' [Num "1", Num "2"], Arith '+' [Num "2", Num "9"]])

        describe "assignment" $ do
            it "variable declaration" $ do
                parsePython "myVar" `shouldBe` (Right $ Start [Var "myVar"])

            it "variables contain can contain numbers" $ do
                parsePython "myVar8plus500" `shouldBe` (Right $ Start [Var "myVar8plus500"])

            it "_ is a valid variable name" $ do
                parsePython "_" `shouldBe` (Right $ Start [Var "_"])

            -- it "beginning with a number is invalid" $ do
            --     parsePython "89badvar" `shouldSatisfy` isLeft

        -- describe "errors" $ do
        --     it "needlessly indented statement" $ do
        --         parsePython " 1 + 2" `shouldSatisfy` isLeft

