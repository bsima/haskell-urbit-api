{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Urbit.Airlock where

import Control.Lens
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.Wreq (FormParam((:=)))
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as Session
-- import qualified Network.Wai.EventSource as Event


data Ship = Ship
  { session :: Maybe Session.Session,
    lastEventId :: Int,
    url :: Url,
    code :: Text,
    sseClient :: Bool
  }
  deriving (Show)


channelUrl :: Ship -> String
channelUrl Ship { url } = url <> "/channel.js"


type Url = String
type App = Text
type Path = Text
type Mark = Text
type ShipName = Text
type Subscription = Text


-- |
nextEventId :: Ship -> Int
nextEventId Ship { lastEventId } = lastEventId + 1


-- |
connect :: Ship -> IO (Wreq.Response ByteString)
connect ship = do
  -- post to <ship>/~/login with json {"password": <code>}
  let params = Wreq.defaults & Wreq.param "password" .~ [(code ship)]
  r <- Wreq.getWith params (url ship <> "/~/login")
  return r


-- |
poke :: Ship -> ShipName -> App -> Mark -> Aeson.Value -> IO (Wreq.Response ByteString)
poke ship shipName app mark json = do
  r <- Wreq.put (channelUrl ship)
         ["id" := nextEventId ship
         , "action" := ("poke"::Text)
         , "ship" := shipName
         , "app" := app
         , "mark" := mark
         , "json" := Aeson.encode json]
  return r


-- |
ack :: Ship -> Int -> IO (Wreq.Response ByteString)
ack ship eventId = do
  r <- Wreq.post (channelUrl ship)
        ["action" := ("ack"::Text)
        , "event-id" := eventId]
  return r


-- TODO
-- ssePipe :: Ship -> IO _
-- ssePipe ship = undefined


-- |
subscribe :: Ship -> ShipName -> App -> Path -> IO Subscription
subscribe = undefined


-- |
unsubscribe :: Ship -> Subscription -> IO ()
unsubscribe = undefined


-- |
delete :: Ship -> IO ()
delete = undefined
