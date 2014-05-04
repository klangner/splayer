{- |
Module : Protocol.Parser
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Parser for server messages.
-}
module Protocol.Parser ( ServerMessage(..)
                       , parseMessage ) where

import Text.ParserCombinators.Parsec
import Model.Types

type Time = Int

data ServerMessage = InitMessage Side UniformNumber PlayMode
                   | ServerParam [(String, String)]
                   | PlayerParam [(String, String)]
                   | SenseBody Time
                   | Error String
                   deriving (Eq)
                   
instance Show ServerMessage where
    show (InitMessage s unum mode) = "(init " ++ show s ++ " " ++ show unum ++ " " ++ show mode ++ ")" 
    show (ServerParam xs) = "(server_param " ++ showParams xs ++ ")"
    show (PlayerParam xs) = "(player_param " ++ showParams xs ++ ")"
    show (SenseBody t) = "(sense_body " ++ show t ++ ")"
    show (Error msg) = "(error " ++ msg ++ ")" 

-- Show params
showParams :: [(String, String)] -> String
showParams [] = ""
showParams ((k,v):xs) = "(" ++ k ++ " " ++ v ++ ")" ++ showParams xs

            
-- | Parse server message                 
parseMessage :: String -> ServerMessage
parseMessage msg = case parse allParsers "" msg of
    Right sm -> sm 
    Left err -> Error (show err)


-- | Try all available message parsers
allParsers :: Parser ServerMessage
allParsers = do
    _ <- char '('
    initMsg <|> serverParamMsg <|> playerParamMsg <|> senseBodyMsg <|> errorMsg
         
                 
-- Parser for init message:
-- (init Side Unum PlayMode)
initMsg :: Parser ServerMessage
initMsg = do
    _ <- tryKeyword "init"
    _ <- space
    s <- side
    _ <- space
    unum <- number
    _ <- space
    mode <- playMode
    return $ InitMessage s unum mode


-- Parser server params message
-- (server_param (ParameterName ParameterValue)*) 
serverParamMsg :: Parser ServerMessage
serverParamMsg = do
    _ <- tryKeyword "server_param"
    _ <- space
    ps <- many1 param
    return $ ServerParam ps


-- Parser player params message
-- (player_param (ParameterName ParameterValue)*) 
playerParamMsg :: Parser ServerMessage
playerParamMsg = do
    _ <- try (string "player_param")
    _ <- space
    ps <- many1 param
    return $ PlayerParam ps
    
    
-- (sense_body Time
--      (view_mode {high | low} {narrow | normal | wide})
--      (stamina Stamina Effort StaminaCapacity)
--      (speed AmountOfSpeed DirectionOfSpeed)
--      (head_angle HeadAngle)
--      (kick KickCount)
--      (dash DashCount)
--      (turn TurnCount)
--      (say SayCount)
--      (turn_neck TurnNeckCount)
--      (catch CatchCount)
--      (move MoveCount)
--      (change_view ChangeViewCount)
--      (arm 
--          (movable ArmMovableCycles) 
--          (expires ArmExpiresCycles) 
--          (target ArmTargetDistance ArmTargetDirection) 
--          (count PointtoCount))
--      (focus 
--          (target {none | l UniformNumber | r UniformNumber}) 
--          (count AttentiontoCount))
--      (tackle 
--          (expires TackleExpiresCycles) 
--          (count TackleCount))
--      (collision {none | [(ball)] [(player)] [(post)]})
--      (foul 
--          (charged FoulChargedCycles) 
--          (card {none | yellow | red}))    
senseBodyMsg :: Parser ServerMessage
senseBodyMsg = do
    _ <- string "sense_body"
    _ <- space
    t <- number
    return $ SenseBody t


-- Parse error message    
--  (error reason)                 
errorMsg :: Parser ServerMessage
errorMsg = do
    _ <- string "error"
    _ <- space
    reason <- text
    return $ Error reason    


tryKeyword :: String -> Parser String
tryKeyword k = try (string k)

    
-- Parse player side
side :: Parser Side
side = do
    ch <- char 'l' <|> char 'r'
    return $ if ch == 'l' then SLeft else SRight
    
-- Parse int number    
number :: Parser Int
number = do
    n <- many alphaNum
    return (read n) 
    
text :: Parser String
text = many1 (alphaNum <|> char '_')    
    
-- Parse play mode    
playMode :: Parser PlayMode
playMode = do
    mode <- text
    return $ case mode of
        "before_kick_off" -> BeforeKickOff
        "play_on" -> PlayOn
        "time_over" -> TimeOver
        "kick_off_l" -> KickOff SLeft
        "kick_off_r" -> KickOff SRight
        "kick_in_l" -> KickIn SLeft
        "kick_in_r" -> KickIn SRight
        "free_kick_l" -> FreeKick SLeft
        "free_kick_r" -> FreeKick SRight
        "coner_kick_l" -> CornerKick SLeft
        "coner_kick_r" -> CornerKick SRight
        "goal_kick_l" -> GoalKick SLeft
        "goal_kick_r" -> GoalKick SRight
        "goal_l" -> Goal SLeft
        "goal_r" -> Goal SRight
        "drop_ball" -> DropBall
        "offside_l" -> Offside SLeft
        "offside_r" -> Offside SRight
        m -> Unknown m
    
    
-- Parse params "(key value)"    
param :: Parser (String, String)
param = do
    _ <- char '('
    k <- text
    _ <- space
    v <- text
    _ <- char ')'
    return (k,v)
    