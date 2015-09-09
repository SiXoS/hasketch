
{-# Language OverloadedStrings #-}
module JSONTypes where

import Data.Aeson
import Control.Applicative((<$>),(<*>))
import Control.Monad(mzero)
import Data.Text(unpack)

import Data.Maybe

type Cmd = String

ack :: Cmd -> Value
ack cmd = object ["cmd" .= cmd]

withData :: ToJSON a => Cmd -> a -> Value
withData cmd dat = object ["cmd" .= cmd, "data" .= dat]

error :: Cmd -> String -> Value
error cmd err = object ["cmd" .= cmd, "error" .= err]

-- | Create a chat response based on sender and message
chat :: String -> String -> Value
chat sender msg = withData "chat" (object ["sender" .= sender, "message" .= msg])

-- | The name, maximum number of users, number of visitors, if the room has a password
data RoomSum = RoomSum String Int Int Bool
  deriving (Show)

instance ToJSON RoomSum where
  toJSON (RoomSum name max users pass) =
    object ["name" .= name
           ,"maxUsers" .= max
           ,"users" .= users
           ,"pass" .= pass]

-- | room name Presentator, user scores, the normal timer length (seconds), the finishing timer length (seconds),
-- current round, max rounds, type of timer, time passed (milliseconds)
data Room = Room String String [Score] Int Int Int Int TimerType Int
  deriving (Show)

instance ToJSON Room where
  toJSON (Room room pres scrs timer finTimer round maxRounds tt timePassed) =
    object ["name" .= room
           ,"presentator" .= pres
            ,"users" .= scrs
            ,"timer" .= timer
            ,"finTimer" .= finTimer
            ,"round" .= round
            ,"maxRound" .= maxRounds
            ,"timerType" .= show tt
            ,"timePassed" .= timePassed]

data TimerType = Finishing | Normal

instance Show TimerType where
  show Finishing = "finishing"
  show Normal = "normal"

-- | The score, the username, and if he guessed correct during the current round
data Score = Score Int String Bool
  deriving (Show)

instance ToJSON Score where
  toJSON (Score scr uname correct) =
    object ["score" .= scr
           ,"user" .= uname
           ,"guessedRight" .= correct]

-- | The user to guess correct and the new score
data CorrectGuess = CorrectGuess String Int
  deriving (Show)

instance ToJSON CorrectGuess where
  toJSON (CorrectGuess uname scr) = object ["user" .= uname, "score" .= scr]

-- | Represents a message from the client. The data can have different
-- representations and can be parsed with fromJSON when the structure is known
-- based on the command
data ClientMessage = ClientMessage {cmd :: String, cdroom :: Maybe Value, dat :: Maybe Value}
  deriving (Show)

instance FromJSON ClientMessage where
  parseJSON (Object v) = ClientMessage <$> v .: "cmd" <*> v.:? "room" <*> v.:? "data"
  parseJSON _ = mzero

data DataString = DString String
  deriving (Show)

instance FromJSON DataString where
  parseJSON (String str) = return $ DString $ unpack str
  parseJSON _ = mzero

data JoinRoom = JoinRoom {room :: String, password :: Maybe String}
  deriving (Show)

instance FromJSON JoinRoom where
  parseJSON (Object v) = JoinRoom <$> v .: "room" <*> v.:? "pass"
  parseJSON _ = mzero

data CreateRoom = CreateRoom {name      :: String
                             ,timer     :: Int
                             ,maxUsers  :: Int
                             ,finTimer  :: Int
                             ,pass      :: Maybe String
                             ,maxRounds :: Int
                             ,wordList  :: String}
  deriving (Show)

instance FromJSON CreateRoom where
  parseJSON (Object v) = CreateRoom <$>
                         v .: "name" <*>
                         v .: "timer" <*>
                         v .: "maxUsers" <*>
                         v .: "finishingTimer" <*>
                         v .:? "password" <*>
                         v .: "maxRounds" <*>
                         v .: "wordList"
  parseJSON _ = mzero

