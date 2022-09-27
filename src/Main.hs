{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (modifyMVar_, newMVar, readMVar)
import Control.Concurrent.MVar (MVar)
import Control.Monad (forM_, forever, when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.UUID (UUID, fromString)
import Data.UUID.V4 (nextRandom)
import GHC.IO (finally)
import qualified Network.WebSockets as WS
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)

data Client = Client
  { clientName :: UUID
  , clientRoom :: UUID
  , clientConn :: WS.Connection
  }

type ServerState = [Client]

validEvents :: [T.Text]
validEvents = ["cheer", "clap", "cry", "laugh", "woof", "quack", "boo", "wolf", "drum", "lame"]

isValidEvent :: T.Text -> Bool
isValidEvent msg = msg `elem`  validEvents

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== clientName client) . clientName)

addClient :: Client -> ServerState -> ServerState
addClient c cs = if clientExists c cs then cs else c : cs

removeClient :: Client -> ServerState -> ServerState
removeClient c = filter (\x -> clientName x /= clientName c)

broadcastLog :: T.Text -> T.Text
broadcastLog m = T.concat [action, m]
  where
    action = if isValidEvent m then "Broadcasting: " else "Skipping: "

-- send message to all clients
broadcast :: UUID -> T.Text -> ServerState -> IO ()
broadcast room msg allClients = do
  T.putStrLn $ broadcastLog msg
  forM_ roomClients (\c -> when (isValidEvent msg) $ WS.sendTextData (clientConn c) msg)
  where
    roomClients = [c | c <- allClients, clientRoom c == room]

roomName :: WS.PendingConnection -> Maybe UUID
roomName pc = let
  req = WS.pendingRequest pc
  room = map w2c $ BS.unpack $ WS.requestPath req
  in
    fromString room

main :: IO ()
main = do
  T.putStrLn "Starting server"
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  let room = roomName pending
  name <- nextRandom
  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn
    case room of
      Just r | otherwise -> flip finally disconnect $ do
        modifyMVar_ state $ \s -> do
          let s' = addClient client s
          broadcast r msg s'
          return s'
        talk client state
        where
          client = Client name r conn
          disconnect = do
            modifyMVar_ state $ \s ->
              return $ removeClient client s
      _ -> do
        WS.sendTextData conn ("No" ::T.Text)

talk :: Client -> MVar ServerState -> IO ()
talk client state = forever $ do
  msg <- WS.receiveData $ clientConn client
  readMVar state >>= broadcast (clientRoom client) msg
