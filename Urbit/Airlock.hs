{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Urbit.Airlock
  ( Ship (..),
    App,
    Mark,
    connect,
    poke,
    ack,
    subscribe,
  )
where

import Control.Lens ()
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID.V4 as UUID
import Network.Http.Client as Client
import Network.Wreq (FormParam ((:=)))
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as Session
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

-- import qualified Network.Wai.EventSource as Event

data Ship = Ship
  { -- | A random string for your channel.
    uid :: Text,
    -- | The `@p` of your ship.
    name :: ShipName,
    -- | Track the latest event we saw (needed for poking).
    lastEventId :: Int,
    -- | Network access point, with port if necessary. Like
    -- 'https://sampel-palnet.arvo.network', or 'http://localhost:8080'.
    url :: Url,
    -- | Login code, `+code` in the dojo. Don't share this publically.
    code :: Text
  }
  deriving (Show)

channelUrl :: Ship -> String
channelUrl Ship {url, uid} = url <> "/~/channel/" <> Text.unpack uid

type Url = String

type App = Text

type Path = Text

type Mark = Text

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
      [ Aeson.object
          [ "id" .= nextEventId ship,
            "action" .= Text.pack "poke",
            "ship" .= shipName,
            "app" .= app,
            "mark" .= mark,
            "json" .= json
          ]
      ]

-- | Acknowledge receipt of a message. (This clears it from the ship's queue.)
ack :: Session.Session -> Ship -> Int -> IO (Wreq.Response L.ByteString)
ack sess ship eventId =
  Session.post
    sess
    (channelUrl ship)
    $ Aeson.toJSON $
      [ Aeson.object
          [ "action" .= Text.pack "ack",
            "event-id" .= eventId
          ]
      ]

-- |
subscribe ::
  Ship ->
  Path ->
  -- | A handler function to receiv the response from the server, e.g.
  -- 'System.IO.Streams.stdout`.
  OutputStream ByteString ->
  IO ()
subscribe ship path outfn = Client.get addr handle
  where
    handle :: Response -> InputStream ByteString -> IO ()
    handle _ i = Streams.connect i outfn
    addr = Char8.pack $ (url ship) ++ "/" ++ Text.unpack path
