{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Urbit.Airlock
-- Copyright: © 2020–present Ben Sima
-- License: MIT
--
-- Maintainer: Ben Sima <ben@bsima.me>
-- Stability: experimental
-- Portability: non-portableo
--
-- === About the Urbit API
--
-- The "Urbit Airlock" API is a command-query API that lets you hook into apps
-- running on your Urbit. You can submit commands and subscribe to responses.
--
-- The Urbit vane @eyre@ is responsible for defining the API interface. The HTTP
-- path to the API is @\/~\/channel\/...@, where we send messages to the global
-- log (called @poke@s) which are then dispatched to the appropriate apps. To
-- receive responses, we stream messages from a path associated with the app,
-- such as @\/mailbox\/~\/~zod\/mc@. Internally, I believe Urbit calls these
-- @wire@s.
--
-- === About this library
--
-- This library helps you talk to your Urbit from Haskell, via HTTP. It handles
-- most of the path, session, and HTTP request stuff automatically. You'll need
-- to know what app and mark (data type) to send to, which path/wire listen to,
-- and the shape of the message. The latter can be found in the Hoon source
-- code, called the @vase@ on the poke arm.
--
-- This library is built on req, conduit, and aeson, all of which are very
-- stable and usable libraries for working with HTTP requests and web data.
-- Released under the MIT License, same as Urbit.
module Urbit.Airlock
  ( -- * Types
    Ship (..),
    Session,

    -- * Functions
    connect,
    poke,
    ack,
    subscribe,
  )
where

import Conduit (ConduitM, runConduitRes, (.|))
import qualified Conduit
import qualified Control.Exception as Exception
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
  { -- | A random string for your channel
    uid :: Text,
    -- | The @\@p@ of your ship
    name :: Text,
    -- | Track the latest event we saw (needed for poking)
    lastEventId :: Int,
    -- | Network access point, with port if necessary, like
    -- @https://sampel-palnet.arvo.network@, or @http://localhost:8080@
    url :: Text,
    -- | Login code, @+code@ in the dojo. Don't share this publically
    code :: Text
  }
  deriving (Show)

channelUrl :: Ship -> Text
channelUrl Ship {url, uid} = url <> "/~/channel/" <> uid

nextEventId :: Ship -> Int
nextEventId Ship {lastEventId} = lastEventId + 1

-- | A wrapper type for the session cookies.
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
      Req.req Req.POST url (Req.ReqBodyUrlEnc body) Req.bsResponse $
        opts

-- | Poke a ship.
poke ::
  Aeson.ToJSON a =>
  -- | Session cookie from 'connect'
  Session ->
  -- | Your ship
  Ship ->
  -- | Name of the ship to poke
  Text ->
  -- | Name of the gall application you want to poke
  Text ->
  -- | The mark of the message you are sending
  Text ->
  a ->
  IO Req.BsResponse
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
        Req.bsResponse
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
ack ::
  -- | Session cookie from 'connect'
  Session ->
  -- | Your ship
  Ship ->
  -- | The event number
  Int ->
  IO Req.BsResponse
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
        Req.bsResponse
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
  -- | Session cookie from 'connect'
  Session ->
  -- | Your ship
  Ship ->
  -- | The path to subscribe to.
  Text ->
  -- | A handler conduit to receive the response from the server, e.g.
  -- @Data.Conduit.Binary.sinkFile "my-file.out"@
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
