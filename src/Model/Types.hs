{- |
Module : Model.Types
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Data types used in simulator model.
-}
module Model.Types where

type TeamName = String

type UniformNumber = Int

-- | Player side
data Side = SLeft | SRight deriving (Eq)

instance Show Side where
    show SLeft = "l"
    show SRight = "r"
    
data PlayMode = BeforeKickOff
              | PlayOn
              | TimeOver
              | KickOff Side
              | KickIn Side
              | FreeKick Side
              | CornerKick Side
              | GoalKick Side
              | Goal Side
              | DropBall
              | Offside Side
              | Unknown String
              deriving (Eq)

instance Show PlayMode where
    show BeforeKickOff = "before_kick_off"
    show PlayOn = "play_on"
    show TimeOver = "time_over"
    show (KickOff side) = "kick_off_" ++ show side 
    show (KickIn side) = "kick_in_" ++ show side
    show (FreeKick side) = "free_kick_" ++ show side
    show (CornerKick side) = "coner_kick_" ++ show side
    show (GoalKick side) = "goal_kick_" ++ show side
    show (Goal side) = "goal_" ++ show side
    show DropBall = "drop_ball"
    show (Offside side) = "offside_" ++ show side
    show (Unknown mode) = "unknown_" ++ mode
              