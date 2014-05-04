{- |
Module : Server.Connection
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Connection to the server
-}
module Server.Connection ( SoccerServer(..) ) where


data SoccerServer = SServer { sender :: String -> IO () }