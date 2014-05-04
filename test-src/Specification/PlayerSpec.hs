module Specification.PlayerSpec (spec) where

import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = describe "After starting the player" do $
    it "will send **init** message" $ property $
        \x -> 10 == (x :: Int)
    it "will start listening to the server messages" $ property $
        \x -> 10 == (x :: Int)
        
