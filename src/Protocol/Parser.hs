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
import World.Types

data ServerMessage = InitMessage Side UniformNumber PlayMode
                   | Error String
                   deriving (Eq, Show)
                 

            
-- | Parse server message                 
parseMessage :: String -> ServerMessage
parseMessage msg = case parse allParsers "" msg of
    Right sm -> sm 
    Left err -> Error (show err)


-- | Try all available message parsers
allParsers :: Parser ServerMessage
allParsers = do
    _ <- char '('
    initMsg <|> errorParser
         
                 
-- | Parser for init message:
--  (init Side Unum PlayMode)
--  (error reason)                 
initMsg :: Parser ServerMessage
initMsg = do
    _<- string "init"
    _<- space
    s <- side
    _<- space
    unum <- number
    _<- space
    mode <- playMode
    return $ InitMessage s unum mode

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
        m -> Unknown m
    
-- Parse error message    
errorParser :: Parser ServerMessage
errorParser = do
    _ <- string "error"
    _ <- space
    reason <- text
    return $ Error reason    