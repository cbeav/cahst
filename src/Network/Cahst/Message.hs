-- | Thanks to https://github.com/thibauts/node-castv2

{-# LANGUAGE OverloadedStrings #-}

module Network.Cahst.Message
     ( HeartbeatMessage(..)
     , ConnectionMessage(..)
     , ReceiverCommand(..)
     , ReceiverMessage(..)
     , MediaCommand(..)
     , MediaMessage(..)
     , getAppAvailability
     , getMediaStatus
     , getStatus
     , launch
     , pause
     , play
     , setVolume
     , setMuted
     , stop
     ) where

import           Network.Cahst.Namespace    (Namespaced (..))
import           Network.Cahst.RequestId    (RequestId)
import           Network.Cahst.UnitInterval (UnitInterval)

import           Data.Aeson                 (ToJSON (..), object, (.=))
import qualified Data.Aeson                 as Aeson
import           Data.Text                  (Text)

data ConnectionMessage = Connect | Close

instance Namespaced ConnectionMessage where
    namespace _ = "urn:x-cast:com.google.cast.tp.connection"

instance ToJSON ConnectionMessage where
    toJSON Connect = object
        [ "type" .= ("CONNECT" :: Text) ]
    toJSON Close = object
        [ "type" .= ("CLOSE" :: Text) ]

data HeartbeatMessage = Ping | Pong

instance Namespaced HeartbeatMessage where
    namespace _ = "urn:x-cast:com.google.cast.tp.heartbeat"

instance ToJSON HeartbeatMessage where
    toJSON Ping = object
        [ "type" .= ("PING" :: Text) ]
    toJSON Pong = object
        [ "type" .= ("PONG" :: Text) ]

newtype MediaCommand = MediaCommand
    { mediaCommandJson :: [(Text, Aeson.Value)] }

instance ToJSON MediaCommand where
    toJSON = object . mediaCommandJson

data MediaMessage = MediaMessage MediaCommand RequestId

instance Namespaced MediaMessage where
    namespace _ = "urn:x-cast:com.google.cast.media"

instance ToJSON MediaMessage where
    toJSON (MediaMessage (MediaCommand pairs) requestId) =
        object $ ("requestId" .= requestId) : pairs

newtype ReceiverCommand = ReceiverCommand
    { receiverCommandJson :: [(Text, Aeson.Value)] }

instance ToJSON ReceiverCommand where
    toJSON = object . receiverCommandJson

data ReceiverMessage = ReceiverMessage ReceiverCommand RequestId

instance Namespaced ReceiverMessage where
    namespace _ = "urn:x-cast:com.google.cast.receiver"

instance ToJSON ReceiverMessage where
    toJSON (ReceiverMessage (ReceiverCommand pairs) requestId) =
        object $ ("requestId" .= requestId) : pairs

launch :: Text -> ReceiverCommand
launch appId = ReceiverCommand
    [ "type" .= ("LAUNCH" :: Text)
    , "appId" .= appId ]

stop :: Text -> ReceiverCommand
stop sessionId = ReceiverCommand
   [ "type" .= ("STOP" :: Text)
   , "sessionId" .= sessionId ]

getStatus :: ReceiverCommand
getStatus = ReceiverCommand
    [ "type" .= ("GET_STATUS" :: Text) ]

getAppAvailability :: [Text] -> ReceiverCommand
getAppAvailability appIds = ReceiverCommand
    [ "type" .= ("GET_APP_AVAILABILITY" :: Text)
    , "appId" .= appIds ]

-- | Volume is between 0 and 1
setVolume :: UnitInterval -> ReceiverCommand
setVolume x = ReceiverCommand
    [ "type" .= ("SET_VOLUME" :: Text)
    , "volume" .= object ["level" .= x] ]

setMuted :: Bool -> ReceiverCommand
setMuted x = ReceiverCommand
    [ "type" .= ("SET_VOLUME" :: Text)
    , "volume" .= object ["muted" .= x] ]

getMediaStatus :: MediaCommand
getMediaStatus = MediaCommand
    [ "type" .= ("GET_STATUS" :: Text) ]

pause :: Text -> MediaCommand
pause mediaSessionId = MediaCommand
  [ "type" .= ("PAUSE" :: Text)
  , "mediaSessionId" .= mediaSessionId
  ]

play :: Text -> MediaCommand
play mediaSessionId = MediaCommand
  [ "type" .= ("PLAY" :: Text)
  , "mediaSessionId" .= mediaSessionId
  ]
