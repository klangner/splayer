module Arbitrary.Model where

import Model.Types
import Test.QuickCheck


instance Arbitrary Side where
    arbitrary = elements [SLeft, SRight]    

instance Arbitrary PlayMode where
    arbitrary = do
        side <- arbitrary
        elements [ BeforeKickOff
                 , PlayOn
                 , TimeOver
                 , KickOff side
                 , KickIn side
                 , FreeKick side
                 , CornerKick side
                 , GoalKick side
                 , Goal side
                 , DropBall
                 , Offside side ]    

        