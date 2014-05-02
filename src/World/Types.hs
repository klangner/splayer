module World.Types where

type TeamName = String

type UniformNumber = Int

data Side = SLeft | SRight deriving (Eq, Show)

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
              deriving (Eq, Show)
