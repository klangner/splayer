import Test.Hspec
import Test.QuickCheck
import Protocol.ParserSpec


main :: IO ()
main = hspec $ 
  describe "Message parser" Protocol.ParserSpec.spec
  