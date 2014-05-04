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
        keys <- listOf1 alphaString
        values <- listOf1 alphaString
        reason <- alphaString
        let params = zip keys values
        sbm <- senseBodyGen
        elements [ InitMessage s n m
                 , PlayerParam params
                 , ServerParam params
                 , sbm
                 , Error reason ]    

-- Create alphanumeric string
alphaString = listOf1 $ elements ['a'..'z']        

-- Sense body generator
senseBodyGen = do
    t <- elements [1..1000000]
    return $ SenseBody t