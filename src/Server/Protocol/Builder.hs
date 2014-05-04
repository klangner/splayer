{- |
Module : Protocol.Builder
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

build messages which can be send from player to server
-}
module Server.Protocol.Builder ( bye
                        , init
                        , reconnect ) where

import Prelude hiding(init)

            
-- | bye message                 
bye :: String
bye = "(bye)"

-- | init message                 
init :: String  -- Team name 
     -> Bool    -- Is goal keeper
     -> String
init t True = "(init " ++ t ++ " (version 14) (goalie))"
init t False = "(init " ++ t ++ " (version 14))"


-- | reconnect message                 
reconnect :: String     -- Team name
          -> Int        -- Player number
          -> String
reconnect t n = "(reconnect " ++ t ++ " " ++ show n ++ ")"