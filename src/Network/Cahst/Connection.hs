{-# LANGUAGE OverloadedStrings #-}

module Network.Cahst.Connection
    ( Connection(..)
    , newConnection
    , newReceiverConnection
    , send
    , recv
    ) where

import qualified Network.Cahst.Message    as M
import           Network.Cahst.Namespace  (Namespaced (..))
import qualified Network.Cahst.Protobuf   as P
import           Network.Cahst.RequestId  (RequestId)

import           Data.Aeson               (ToJSON)
import qualified Data.Aeson
import qualified Data.Aeson.Text
import qualified Data.Binary              as Binary
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Default
import qualified Data.Int                 as Int
import           Data.IORef               (IORef, atomicModifyIORef', newIORef)
import           Data.Monoid              ((<>))
import           Data.Serialize.Put       (runPutLazy)
import qualified Data.Text                as T
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder

import qualified Network.Socket           as Sock
import qualified Network.TLS              as TLS
import           Network.TLS.Extra.Cipher (ciphersuite_all)

import           System.Random            (randomIO)

data Connection = Connection
    { connTlsContext    :: TLS.Context
    , connDestinationId :: T.Text
    , connRequestId     :: IORef RequestId
    }

newReceiverConnection :: T.Text -> IO Connection
newReceiverConnection ip = newConnection ip "receiver-0"

newConnection :: T.Text -> T.Text -> IO Connection
newConnection ip destinationId = do
    addr <- getAddr ip
    s <- Sock.socket Sock.AF_INET Sock.Stream 0
    Sock.connect s addr
    ctx <- TLS.contextNew s tlsParams
    _ <- TLS.handshake ctx
    firstRequestId <- randomIO
    requestIdRef <- newIORef firstRequestId
    return $ Connection ctx destinationId requestIdRef

tlsParams :: TLS.ClientParams
tlsParams = (TLS.defaultParamsClient "" BS.empty)
    { TLS.clientSupported = tlsCiphers
    , TLS.clientHooks = tlsHooks
    -- I don't know or care what the server name is, ignoring security
    , TLS.clientUseServerNameIndication = False }
  where
    -- Support any ciphers, who cares
    tlsCiphers = def { TLS.supportedCiphers = ciphersuite_all }

    -- Don't verify the server cert at all. It's self-signed.
    tlsHooks = def { TLS.onServerCertificate = \_ _ _ _ -> return [] }

getAddr :: T.Text -> IO Sock.SockAddr
getAddr ip = do
    xs <- Sock.getAddrInfo Nothing (Just $ T.unpack ip) (Just "8009")
    return $ Sock.addrAddress $ head xs

class Send a where
    send :: Connection -> a -> IO RequestId

nextRequestId :: Connection -> IO RequestId
nextRequestId conn =
    atomicModifyIORef' (connRequestId conn)
                       (\i -> (wrapSucc i, i))

instance Send M.ConnectionMessage where
    send conn@Connection{..} x = do
      TLS.sendData connTlsContext (jsonPayload x connDestinationId)
      nextRequestId conn

instance Send M.HeartbeatMessage where
    send conn@Connection{..} x = do
      TLS.sendData connTlsContext (jsonPayload x connDestinationId)
      nextRequestId conn

instance Send M.ReceiverMessage where
    send Connection{..} x@(M.ReceiverMessage _ requestId) = do
      _ <- TLS.sendData connTlsContext (jsonPayload x connDestinationId)
      pure requestId

instance Send M.ReceiverCommand where
    send conn x = do
        requestId <- nextRequestId conn
        send conn $ M.ReceiverMessage x requestId
        pure requestId

instance Send M.MediaMessage where
    send Connection{..} x@(M.MediaMessage _ requestId) = do
      _ <- TLS.sendData connTlsContext (jsonPayload x connDestinationId)
      pure requestId

instance Send M.MediaCommand where
    send conn x = do
        requestId <- nextRequestId conn
        send conn $ M.MediaMessage x requestId
        pure requestId

wrapSucc :: (Bounded a, Enum a, Eq a) => a -> a
wrapSucc a = if a == maxBound then minBound else succ a

recv :: Connection -> IO BS.ByteString
recv conn = TLS.recvData $ connTlsContext conn

prependLength :: LBS.ByteString -> LBS.ByteString
prependLength body = len <> body where
    len = Binary.encode ((fromIntegral $ LBS.length body) :: Int.Int32)

jsonPayload :: (Namespaced a, ToJSON a) => a -> T.Text -> LBS.ByteString
jsonPayload m dest = prependLength $ runPutLazy $ P.encodeMessage $ jsonCastMessage m dest

jsonCastMessage :: (Namespaced a, ToJSON a) => a -> T.Text -> P.CastMessage
jsonCastMessage m dest = P.CastMessage
    { P.protocolVersion = P.putField P.CASTV2_1_0
    , P.sourceId        = P.putField "sender-0"
    , P.destinationId   = P.putField dest
    , P.namespace       = P.putField $ namespace m
    , P.payloadType     = P.putField P.StringPayloadType
    , P.payloadUtf8     = P.putField $ Just $ encodeJsonText m
    , P.payloadBinary   = P.putField Nothing
    }

encodeJsonText :: ToJSON a => a -> T.Text
encodeJsonText = Data.Text.Lazy.toStrict .
                 Data.Text.Lazy.Builder.toLazyText .
                 Data.Aeson.Text.encodeToTextBuilder .
                 Data.Aeson.toJSON
