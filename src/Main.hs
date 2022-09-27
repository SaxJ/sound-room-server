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
validEvents = ["astonished", "cheer", "clap", "cry", "laugh", "woof", "quack", "boo", "wolf", "drum", "lame"]

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

roomName_ :: WS.PendingConnection -> String
roomName_ pc = let
  req = WS.pendingRequest pc
  room = map w2c $ BS.unpack $ WS.requestPath req
  in
    [c | c <- room, c /= '/']

roomName :: WS.PendingConnection -> Maybe UUID
roomName pc = fromString $ roomName_ pc

main :: IO ()
main = do
  T.putStrLn "Starting server"
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application state

connectionOpened :: WS.Connection -> UUID -> MVar ServerState -> UUID -> IO ()
connectionOpened conn room state name =
  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn
    case room of
      _ | otherwise -> flip finally disconnect $ do
        modifyMVar_ state $ \s -> do
          let s' = addClient client s
          broadcast room msg s'
          return s'
        talk client state
        where
          client = Client name room conn
          disconnect = do
            modifyMVar_ state $ \s ->
              return $ removeClient client s

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  let room = roomName pending
  T.putStrLn $ T.pack $ roomName_ pending
  name <- nextRandom
  case room of
    Just r -> do
      conn <- WS.acceptRequest pending
      connectionOpened conn r state name
    _ -> WS.rejectRequest pending "Nope"

talk :: Client -> MVar ServerState -> IO ()
talk client state = forever $ do
  msg <- WS.receiveData $ clientConn client
  readMVar state >>= broadcast (clientRoom client) msg
