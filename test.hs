{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException (..), try)
import Control.Lens ((^?))
import Data.Aeson (KeyValue ((.=)))
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Word as Word
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as Session
import qualified Numeric
import qualified System.Environment as Env
import qualified System.Random as Random
import Urbit.Airlock

main :: IO ()
main = do
  port <- Env.getEnv "PORT"
  let ship = fakezod port
  sess <- Session.newSession

  testing "ship connection" $
    isJust <$> do
      r <- connect sess ship
      return $ r ^? Wreq.responseBody

  testing "poke ship" $
    isJust <$> do
      uid <- UUID.nextRandom
      r <-
        poke sess ship "zod" "chat-hook" "json" $
          Aeson.object
            [ "message"
                .= Aeson.object
                  [ "path" .= Text.pack "/~/~zod/mc",
                    "envelope"
                      .= Aeson.object
                        [ "uid" .= UUID.toText uid,
                          "number" .= (1 :: Int), -- FIXME: should this be lastEventId?
                          "author" .= Text.pack "~zod",
                          "when" .= (1602118786225 :: Int),
                          "letter" .= Aeson.object ["text" .= Text.pack "hello world!"]
                        ]
                  ]
            ]
      return $ r ^? Wreq.responseBody

  testing "ack" $
    isJust <$> do
      r <- ack sess ship 1
      return $ r ^? Wreq.responseBody


fakezod :: String -> Ship
fakezod port =
  Ship
    { uid = "0123456789abcdef",
      name = "zod",
      lastEventId = 1,
      url = "http://localhost:" ++ port,
      code = "lidlut-tabwed-pillex-ridrup",
      sseClient = False
    }


-- | Poor man's testing framework
testing :: Text -> IO Bool -> IO ()
testing description f =
  (putStrLn $ replicate 80 '-') >> try f >>= \case
    Left (err :: SomeException) -> do
      Text.IO.putStrLn $ "FAIL: " <> description
      putStrLn $ show err
    Right False -> do
      Text.IO.putStrLn $ "FAIL: " <> description
      putStrLn $ "expected True, got False"
    Right True ->
      Text.IO.putStrLn $ "PASS: " <> description
