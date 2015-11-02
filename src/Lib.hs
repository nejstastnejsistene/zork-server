module Lib
    ( someFunc
    ) where

import BasePrelude
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text
import Network.HTTP.Types.URI
import Network.WebSockets

import Zork

someFunc :: IO ()
someFunc = do
  host <- fromMaybe "0.0.0.0"
      <$> lookupEnv "HOST"
  port <- fromMaybe 3030
      <$> fmap read
      <$> lookupEnv "PORT"
  runServer host port serverApp

protocolToDatFile :: [(BS.ByteString, FilePath)]
protocolToDatFile =
  [ ("zork1", "DATA/ZORK1.DAT")
  , ("zork2", "DATA/ZORK2.DAT")
  , ("zork3", "DATA/ZORK3.DAT")
  ]

serverApp :: ServerApp
serverApp pending = case liftM2 (,) userId' datFile' of
  Left reason -> rejectRequest pending (BS.pack reason)
  Right (userId, datFile) -> do
    conn <- acceptRequest pending
    (p, output) <- startZork datFile userId
    sendTextData conn (LBS.pack output)
    (`finally` terminate p ) $ forever $ receiveData conn
                                     >>= inputCommand p . unpack
                                     >>= sendTextData conn . pack
  where
    RequestHead{..} = pendingRequest pending
    userId' = case decodePath requestPath of
      ([x], _) -> Right (unpack x)
      _ -> Left "try connecting to \"/<userid>\""
    datFile' = maybe (Right "DATA/ZORK1.DAT") Right $ do
      protocol <- lookup "Sec-WebSocket-Protocol" requestHeaders
      lookup protocol protocolToDatFile
