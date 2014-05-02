import Test.Hspec
import Test.QuickCheck
import Protocol.ParserSpec


main :: IO ()
main = hspec $ do
  describe "Message parser" Protocol.ParserSpec.spec
  