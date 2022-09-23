{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.UUID (UUID)
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (forM_, forever)
import Control.Concurrent ( newMVar, readMVar, modifyMVar_ )
import Control.Concurrent.MVar (MVar)
import Data.UUID.V4 (nextRandom)
import GHC.IO (finally)

type Client = (UUID, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient c cs = if clientExists c cs then cs else c:cs

removeClient :: Client -> ServerState -> ServerState
removeClient c = filter (\x -> fst x /= fst c)

-- send message to all clients
broadcast :: T.Text -> ServerState -> IO ()
broadcast msg clients = do
  T.putStrLn msg
  forM_ clients (\(_, conn) -> WS.sendTextData conn msg)

main :: IO ()
main = do
  T.putStrLn "Starting server"
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  name <- nextRandom
  WS.withPingThread conn 30 (return ()) $ do
    msg <- WS.receiveData conn
    case msg of
      _ | otherwise -> flip finally disconnect $ do
                          modifyMVar_ state $ \s -> do
                            let s' = addClient client s
                            broadcast msg s'
                            return s'
                          talk client state
        where
          client = (name, conn)
          disconnect = do
            modifyMVar_ state $ \s ->
              return $ removeClient client s

talk :: Client -> MVar ServerState -> IO ()
talk (_, conn) state = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast msg
