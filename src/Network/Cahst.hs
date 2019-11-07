{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Cahst (main, togglePlay) where

import ClassyPrelude

import qualified Network.Cahst.Connection as C
import qualified Network.Cahst.Message    as M
import qualified Network.Cahst.Protobuf   as P

import           Control.Lens             ((^?), (&), preview)
import           Data.Aeson
import           Data.Aeson.Lens          (key, nth, _Number, _String)
import qualified Data.Binary              as Binary
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Int                 as Int
import           Data.Maybe               (listToMaybe)
import           Data.Monoid              ((<>))
import           Data.ProtocolBuffers     (getField)
import           Data.Scientific          (toBoundedInteger)
import qualified Data.Serialize.Get       as G

main :: IO ()
main = togglePlay "192.168.0.142"

togglePlay :: Text -> IO ()
togglePlay ip = do
    c <- C.newReceiverConnection ip
    C.send c M.Connect
    C.send c M.getStatus

    let
      extractStatus mResponse = do
        response    <- mResponse
        app         <- response ^? (key "status" . key "applications" . nth 0)
        transportId <- app ^? key "transportId" . _String
        appId       <- app ^? key "appId" . _String
        pure (appId, transportId)
      extractMediaStatus mResponse = do
        response    <- mResponse
        session     <- response ^? key "status" . nth 0
        state       <- session  ^? key "playerState" . _String
        (sessionId :: Int) <- toBoundedInteger =<< session  ^? key "mediaSessionId" . _Number
        pure (tshow sessionId, state)

    mStatus <- extractStatus <$> waitForResponse "RECEIVER_STATUS" c mempty

    for_ mStatus $ \(appId, transportId) -> do
      c1 <- C.newConnection ip transportId
      C.send c1 M.Connect
      C.send c1 M.getStatus
      mMediaStatus <- extractMediaStatus <$> waitForResponse "MEDIA_STATUS" c1 mempty

      for_ mMediaStatus $ \(sessionId, state) ->
        case state of
          "PAUSED" -> C.send c1 (M.play sessionId)
          _        -> C.send c1 (M.pause sessionId)

waitForResponse :: Text -> C.Connection -> BS.ByteString -> IO (Maybe Value)
waitForResponse event c bs1 = do
    bs2 <- C.recv c

    let
      bs = bs1 <> bs2
      len = fromIntegral $ getLengthPrefix bs
      (body, rest) = BS.splitAt len $ BS.drop 4 bs

    if BS.length body /= len
      then
        waitForResponse event c bs
      else
        case G.runGet P.decodeMessage body of
          Left err -> pure Nothing
          Right result -> do
            let
              mMsg :: Maybe (Text, Value)
              mMsg = do
                field   <- getField $ P.payloadUtf8 result
                msg     <- decodeStrict $ encodeUtf8 field
                msgType <- msg ^? key "type" . _String
                pure (msgType, msg)

              loopUnlessMatch :: Maybe (Text, Value) -> IO (Maybe Value)
              loopUnlessMatch Nothing = pure Nothing
              loopUnlessMatch (Just (e, msg))
                | e == event = pure $ Just msg
                | otherwise  = waitForResponse event c rest

            loopUnlessMatch mMsg

getLengthPrefix :: BS.ByteString -> Int.Int32
getLengthPrefix bs = Binary.decode firstBytes
  where firstBytes :: LBS.ByteString
        firstBytes = LBS.take 4 $ LBS.fromStrict bs
