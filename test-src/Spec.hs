import Test.Hspec
import Test.QuickCheck
import Server.Protocol.ParserSpec


main :: IO ()
main = hspec $ 
  describe "Message parser" Server.Protocol.ParserSpec.spec
  