{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Urbit.Airlock
  ( Ship(..),
    App,
    Mark,
    connect,
    poke,
  ) where

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
    -- | Track the latest event we saw (needed for poking).
    lastEventId :: Int,
    -- | Internet-facing access point, like 'http://sampel-palnet.arvo.network'
    url :: Url,
    -- | Login code, `+code` in the dojo. Don't share this publically.
    code :: Text,
    -- | Not implemented yet...
    sseClient :: Bool,
    -- | The `@p` of the ship
    name :: Text
  }
  deriving (Show)


channelUrl :: Ship -> String
channelUrl Ship { url } = url <> "/channel.js"


type Url = String
type App = Text
type Path = Text
type Mark = Text
type Subscription = Text


-- |
nextEventId :: Ship -> Int
nextEventId Ship { lastEventId } = lastEventId + 1


-- | Connect and login to the ship.
connect :: Ship -> IO (Wreq.Response ByteString)
connect ship = do
  -- post to <ship>/~/login with json {"password": <code>}
  let params = Wreq.defaults & Wreq.param "password" .~ [(code ship)]
  r <- Wreq.getWith params (url ship <> "/~/login")
  return r


-- | Poke a ship.
poke ::
  Ship ->
  -- | Which gall application are you trying to poke?
  App ->
  -- | What mark should be applied to the data you are sending?
  Mark ->
  Aeson.Value ->
  IO (Wreq.Response ByteString)
poke ship app mark json = do
  r <- Wreq.put (channelUrl ship)
         ["id" := nextEventId ship
         , "action" := ("poke" :: Text)
         , "ship" := (name ship)
         , "app" := app
         , "mark" := mark
         , "json" := Aeson.encode json
         ]
  return r


-- | Acknowledge receipt of a message. (This clears it from the ship's queue.)
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
subscribe :: Ship -> App -> Path -> IO Subscription
subscribe = undefined


-- |
unsubscribe :: Ship -> Subscription -> IO ()
unsubscribe = undefined


-- |
delete :: Ship -> IO ()
delete = undefined
