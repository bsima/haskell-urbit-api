{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException (..), try)
import Data.Aeson (KeyValue ((.=)))
import qualified Data.Aeson as Aeson
import qualified Data.Conduit.Binary
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified System.Environment as Env
import qualified System.Exit as Exit
import Urbit.Airlock

main :: IO ()
main = do
  port <- Text.pack <$> Env.getEnv "PORT"
  let ship = fakezod port
  sess <- connect ship
  testing "ship connection" $
    connect ship >> return True

  testing "poke ship" $
    do
      uuid <- UUID.nextRandom
      _ <-
        poke sess ship "zod" "chat-hook" "json" $
          Aeson.object
            [ "message"
                .= Aeson.object
                  [ "path" .= Text.pack "/~/~zod/mc",
                    "envelope"
                      .= Aeson.object
                        [ "uid" .= UUID.toText uuid,
                          "number" .= (1 :: Int), -- FIXME: should this be lastEventId?
                          "author" .= Text.pack "~zod",
                          "when" .= (1602118786225 :: Int),
                          "letter" .= Aeson.object ["text" .= Text.pack "hello world from haskell!"]
                        ]
                  ]
            ]
      return $ True

  testing "ack" $
    ack sess ship 1 >> return True

  -- These tests are basically just checking that a connection happens and
  -- doesn't throw, I need to pull in async in order to check for more
  -- correctness. Ideally: subscribe, send a message, then read the message to
  -- ensure its the same as the one sent. But maybe this is already tested in
  -- urbit core?

  testing "subscribe" $ do
    _ <- subscribe sess ship "/mailbox/~/~zod/mc" Data.Conduit.Binary.sinkLbs
    return True

fakezod :: Text -> Ship
fakezod port =
  Ship
    { uid = "0123456789abcdef",
      name = "zod",
      lastEventId = 1,
      url = "http://localhost:" <> port,
      code = "lidlut-tabwed-pillex-ridrup"
    }

-- | Poor man's testing framework
testing :: Text -> IO Bool -> IO ()
testing description f =
  (putStrLn $ replicate 80 '-') >> try f >>= \case
    Left (err :: SomeException) -> do
      Text.IO.putStrLn $ "FAIL: " <> description
      Exit.die $ show err
    Right False -> do
      Text.IO.putStrLn $ "FAIL: " <> description
      Exit.die "expected True, got False"
    Right True ->
      Text.IO.putStrLn $ "PASS: " <> description
