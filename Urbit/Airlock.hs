{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Urbit.Airlock
  ( Ship (..),
    App,
    Mark,
    connect,
    poke,
    ack,
  )
where

import Control.Lens ()
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Network.Wreq (FormParam ((:=)))
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as Session

-- import qualified Network.Wai.EventSource as Event

data Ship = Ship
  { -- | A random string for your channel.
    uid :: Text,
    -- | The `@p` of your ship.
    name :: ShipName,
    -- | Track the latest event we saw (needed for poking).
    lastEventId :: Int,
    -- | Internet-facing access point, like 'http://sampel-palnet.arvo.network'
    url :: Url,
    -- | Login code, `+code` in the dojo. Don't share this publically.
    code :: Text,
    -- | Not implemented yet...
    sseClient :: Bool
  }
  deriving (Show)

channelUrl :: Ship -> String
channelUrl Ship {url} = url <> "/~/channel/1234567890abcdef"

type Url = String

type App = Text

type Path = Text

type Mark = Text

type Subscription = Text

-- | The `@p` for the ship (no leading ~).
type ShipName = Text

-- |
nextEventId :: Ship -> Int
nextEventId Ship {lastEventId} = lastEventId + 1

-- | Connect and login to the ship.
connect :: Session.Session -> Ship -> IO (Wreq.Response L.ByteString)
connect sess ship =
  Session.post sess (url ship <> "/~/login") ["password" := (code ship)]

-- | Poke a ship.
poke ::
  Aeson.ToJSON a =>
  Session.Session ->
  Ship ->
  -- | To what ship will you send the poke?
  ShipName ->
  -- | Which gall application are you trying to poke?
  App ->
  -- | What mark should be applied to the data you are sending?
  Mark ->
  a ->
  IO (Wreq.Response L.ByteString)
poke sess ship shipName app mark json =
  Session.post
    sess
    (channelUrl ship)
    $ Aeson.toJSON $
      Aeson.object
        [ "id" .= nextEventId ship,
          "action" .= Text.pack "poke",
          "ship" .= shipName,
          "app" .= app,
          "mark" .= mark,
          "json" .= json
        ]

-- | Acknowledge receipt of a message. (This clears it from the ship's queue.)
ack :: Session.Session -> Ship -> Int -> IO (Wreq.Response L.ByteString)
ack sess ship eventId =
  Session.post
    sess
    (channelUrl ship)
    $ Aeson.toJSON $
      Aeson.object
        [ "action" .= Text.pack "ack",
          "event-id" .= eventId
        ]

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
