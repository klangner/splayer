import Test.Hspec
import Test.QuickCheck
import Specification.ProtocolSpec as ProtocolSpec


main :: IO ()
main = hspec $ 
  describe "Protocol" ProtocolSpec.spec
  