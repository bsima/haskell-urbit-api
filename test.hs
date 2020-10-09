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
import qualified Network.Wreq as Wreq
import qualified Numeric
import qualified System.Random as Random
import Urbit.Airlock

main :: IO ()
main = do
  let ship = fakezod

  testing "ship connection" $
    isJust <$> do
      r <- connect ship
      return $ r ^? Wreq.responseBody

  testing "poke ship" $
    isJust <$> do
      n <- Random.randomRIO (0, 100)
      r <-
        poke ship "zod" "chat-hook" "json" $
          Aeson.object
            [ "message"
                .= Aeson.object
                  [ "path" .= Text.pack "/~/~zod/mc",
                    "envelope"
                      .= Aeson.object
                        [ "uid" .= (Text.pack $ base32 n),
                          "number" .= lastEventId ship,
                          "author" .= Text.pack "~zod",
                          "when" .= Text.pack "1602118786225.497", -- int(time.time() * 1000)
                          "letter" .= Aeson.object ["text" .= Text.pack "hello world!"]
                        ]
                  ]
            ]
      return $ r ^? Wreq.responseBody

  testing "ack" $
    isJust <$> do
      r <- ack ship 1
      return $ r ^? Wreq.responseBody

fakezod :: Ship
fakezod =
  Ship
    { session = Nothing,
      name = "zod",
      lastEventId = 0,
      url = "http://localhost:8081",
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

base32 :: Integer -> String
base32 n = Numeric.showIntAtBase 32 Char.intToDigit n ""

{-
s = baseconvert.base(random.getrandbits(128), 10, 32, string=True).lower()
uid = '0v' + '.'.join(s[i:i+5] for i in range(0, len(s), 5))[::-1]

"path": "/~/~zod/mc",
"envelope": {"uid": uid,
             "number": 1,
             "author": "~zod",
             "when": ,
             "letter": {"text": "hello world!"}}

-}
