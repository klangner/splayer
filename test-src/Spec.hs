import Test.Hspec
import Test.QuickCheck
import Protocol.ParserSpec


main :: IO ()
main = hspec $ do
  describe "Folder utilities" Protocol.ParserSpec.spec
  