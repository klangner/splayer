module Protocol.ParserSpec (spec) where

import World.Types
import Protocol.Parser
import Test.Hspec


spec :: Spec
spec = do

  describe "init message " $ 
    it "(error connection_failed)" $ do
        let msg = parseMessage "(error connection_failed)"
        msg `shouldBe` Error "connection_failed"


  describe "init message" $ 
    it "(init l 3 before_kick_off)" $ do
        let msg = parseMessage "(init l 3 before_kick_off)"
        msg `shouldBe` InitMessage SLeft 3 BeforeKickOff
        