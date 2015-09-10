{-# LANGUAGE OverloadedStrings #-}
module Connection where

import           Control.Monad      (forever)
import           Control.Exception  (finally) 
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Network.WebSockets as WS
import qualified Server as S
import qualified JSONTypes as JS
import Data.Aeson(Value,encode,decode)
import Control.Concurrent.MVar
import Control.Monad.Except

data ServerState = ST {ids :: [Int]
                      ,clients :: [(Int,WS.Connection)]}

emptyServerState :: ServerState
emptyServerState = ST [0..] []

unpack = T.unpack . T.decodeUtf8

application :: MVar ServerState -> MVar S.Corridor -> WS.ServerApp
application state corr pending = do
  conn <- WS.acceptRequest pending
  st <- takeMVar state
  putMVar state st{ids = tail $ ids st,clients = (head $ ids st,conn):clients st}
  let uid = (head $ ids st)
  flip finally (disconnect state corr uid) $ handle state corr uid conn

disconnect :: MVar ServerState -> MVar S.Corridor -> Int -> IO ()
disconnect state corr uid = do
  c <- takeMVar corr
  st <- takeMVar state
  (corr',tsm) <- S.disconnect uid c
  let st' = st{ids = uid:(ids st), clients = filter ((/= uid) . fst) $ clients st}
  putMVar state st'
  putMVar corr corr'
  sendTransmission tsm undefined $ clients st' -- disconnect should never try to send something to the sender so undefined shouldn't be evaluated
  

handle :: MVar ServerState -> MVar S.Corridor -> Int -> WS.Connection -> IO ()
handle state corr id conn = forever $ do
  txt <- WS.receiveData conn
  case decode txt :: Maybe Value of
   Nothing -> putStrLn $ "Couldn't decode: " ++ unpack txt
   Just json -> do
     corr' <- takeMVar corr
     excpt <- runExceptT $ S.handleRequest corr' json id
     case excpt of
      Left e -> putStrLn ("failed: " ++ e) >> putMVar corr corr'
      Right (corr'',trans) -> do
        putMVar corr corr''
        cs <- readMVar state >>= return.clients
        sendTransmission trans conn cs

sendTransmission :: [(S.Transmission Value)] -> WS.Connection -> [(Int,WS.Connection)] -> IO ()
sendTransmission toSend sender cs = send toSend
  where
    send ((S.Sender,val):ts) = WS.sendTextData sender (encode val) >> send ts
    send ((S.User id,val):ts) = send ((S.All [id],val):ts)
    send ((S.All ids,val):ts) = ((sequence_ $ map (flip WS.sendTextData (encode val) . snd) $ filter (flip elem ids . fst) cs) :: IO ()) >> send ts
    send [] = return ()

main :: IO ()
main = do 
  state <- newMVar emptyServerState
  corr <- newMVar S.emptyCorridor
  WS.runServer "0.0.0.0" 8181 $ application state corr
