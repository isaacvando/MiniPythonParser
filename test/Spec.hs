import ParsePython
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Python parser tests" $ do
        it "empty input" $ do
            parsePython "" `shouldBe` (Right $ Node Start []) 
        
        describe "arithmetic" $ do
            it "simple addition" $ do
                parsePython "18 + 987" `shouldBe` (Right $ Node Start [Node (Arithmetic "+") [Node (Number "18") [], Node (Number "987") []]])

