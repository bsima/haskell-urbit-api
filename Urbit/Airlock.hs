{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import Conduit (ConduitM, runConduitRes, (.|))
import qualified Conduit
import qualified Control.Exception as Exception
import Control.Lens ()
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req ((=:))
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Req.Conduit as Req
import qualified Text.URI as URI

-- | Some information about your ship needed to establish connection.
data Ship = Ship
  { -- | A random string for your channel.
    uid :: Text,
    -- | The `@p` of your ship.
    name :: ShipName,
    -- | Track the latest event we saw (needed for poking).
    lastEventId :: Int,
    -- | Network access point, with port if necessary. Like
    -- 'https://sampel-palnet.arvo.network', or 'http://localhost:8080'.
    url :: Text,
    -- | Login code, `+code` in the dojo. Don't share this publically.
    code :: Text
  }
  deriving (Show)

channelUrl :: Ship -> Text
channelUrl Ship {url, uid} = url <> "/~/channel/" <> uid

type App = Text

type Path = Text

type Mark = Text

-- | The `@p` for the ship (no leading ~).
type ShipName = Text

nextEventId :: Ship -> Int
nextEventId Ship {lastEventId} = lastEventId + 1

type Session = HTTP.CookieJar

-- | Connect and login to the ship.
connect :: Ship -> IO Session
connect ship =
  Req.useURI <$> (URI.mkURI $ url ship <> "/~/login") >>= \case
    Nothing -> error "could not parse ship url"
    Just uri ->
      Req.runReq Req.defaultHttpConfig $
        Req.responseCookieJar <$> either con con uri
  where
    body = "password" =: (code ship)
    con (url, opts) =
      Req.req Req.POST url (Req.ReqBodyUrlEnc body) Req.ignoreResponse $
        opts

-- | Poke a ship.
poke ::
  Aeson.ToJSON a =>
  Session ->
  Ship ->
  -- | To what ship will you send the poke?
  ShipName ->
  -- | Which gall application are you trying to poke?
  App ->
  -- | What mark should be applied to the data you are sending?
  Mark ->
  a ->
  IO Req.IgnoreResponse
poke sess ship shipName app mark json =
  Req.useURI <$> (URI.mkURI $ channelUrl ship) >>= \case
    Nothing -> error "could not parse ship url"
    Just uri ->
      Req.runReq Req.defaultHttpConfig $
        either con con uri
  where
    con (url, opts) =
      Req.req
        Req.POST
        url
        (Req.ReqBodyJson body)
        Req.ignoreResponse
        $ opts <> Req.cookieJar sess
    body =
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
ack :: Session -> Ship -> Int -> IO Req.IgnoreResponse
ack sess ship eventId =
  Req.useURI <$> (URI.mkURI $ channelUrl ship) >>= \case
    Nothing -> error "could not parse ship url"
    Just uri ->
      Req.runReq Req.defaultHttpConfig $
        either con con uri
  where
    con (url, opts) =
      Req.req
        Req.POST
        url
        (Req.ReqBodyJson body)
        Req.ignoreResponse
        $ opts <> Req.cookieJar sess
    body =
      [ Aeson.object
          [ "action" .= Text.pack "ack",
            "event-id" .= eventId
          ]
      ]

instance Req.MonadHttp (ConduitM i o (Conduit.ResourceT IO)) where
  handleHttpException = Conduit.liftIO . Exception.throwIO

-- | Subscribe to ship events on some path.
subscribe ::
  Session ->
  Ship ->
  Path ->
  -- | A handler conduit to receive the response from the server, e.g.
  -- 'Data.Conduit.Binary.sinkFile "my-file.out"'.
  ConduitM ByteString Conduit.Void (Conduit.ResourceT IO) a ->
  IO a
subscribe sess ship path fn =
  Req.useURI <$> (URI.mkURI $ url ship <> "/" <> path) >>= \case
    Nothing -> error "could not parse ship url"
    Just uri -> runConduitRes $ do
      either con con uri $ \request manager ->
        Conduit.bracketP
          (HTTP.responseOpen request manager)
          HTTP.responseClose
          Req.responseBodySource
          .| fn
  where
    con (url, opts) =
      Req.req'
        Req.POST
        url
        Req.NoReqBody
        $ opts <> Req.cookieJar sess
