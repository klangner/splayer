module Protocol.ParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Model.Types
import Protocol.Parser
import Model.TypesArbitrary


spec :: Spec
spec = describe "message parser" $
    it "parses all messages" $ property $
        \x -> parseMessage (show x) == (x :: ServerMessage)
        
        
instance Arbitrary ServerMessage where
    arbitrary = do
        s <- arbitrary
        n <- elements [1..11]
        m <- arbitrary
        keys <- listOf1 $ listOf1 $ elements ['a'..'z']
        values <- listOf1 $ listOf1 $ elements ['a'..'z']
        let params = zip keys values
        elements [ InitMessage s n m
                 , PlayerParam params
                 --, ServerParam params
                 , Error "reason" ]    

        