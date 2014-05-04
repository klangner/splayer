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


-- | Player model
data Player = Player TeamName UniformNumber Bool


-- | This function will start new player. 
-- It should be run in separate thread, 
-- because it will block on receiving messages from the server.
runPlayer :: Player -> SoccerServer -> IO ()
runPlayer _ _ = putStrLn "Player finished"

