import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "Should concatenate every line with a newlinw" $ do
      (formatGrid $ MkGrid ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"

