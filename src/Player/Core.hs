{- |
Module : Player.Core
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Core functionality of player agent.
-}
module Player.Core where

import Model.Types( TeamName, UniformNumber )
import Server.Connection( SoccerServer )
import qualified Server.Protocol.Builder as Msg (init)


-- | Player position
data PlayerSide = Left | CenterLeft | Center | CenterRight | Right deriving (Eq)

data PlayerPos = Goalkeeper
               | Defender PlayerSide
               | Midfielder PlayerSide
               | Forward PlayerSide
               deriving (Eq)
               
-- | Player model
data Player = Player { playerTeam :: TeamName
                     , playerNum :: UniformNumber
                     , playerPos :: PlayerPos }


-- | This function will start new player. 
-- It should be run in separate thread, 
-- because it will block on receiving messages from the server.
runPlayer :: Player -> SoccerServer -> IO ()
runPlayer p _ = putStrLn $ Msg.init (playerTeam p) (playerPos p == Goalkeeper)

