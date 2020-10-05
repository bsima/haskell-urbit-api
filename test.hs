{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException (..), try)
import Data.Aeson (KeyValue ((.=)))
import qualified Data.Aeson as Aeson
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Urbit.Airlock

main :: IO ()
main = do
  let ship = fakezod
  testing "ship connection" $ isJust <$> connect ship
  cookie <- fromJust <$> connect ship

  testing "poke ship" $ isJust <$>
    (poke ship "zod" "chat-hook" "json" $
      Aeson.object
        [ "message"
            .= Aeson.object
              [ "path" .= Text.pack "/~/~zod/mc",
                "envelope"
                  .= Aeson.object
                    [ "uid" .= Text.pack "FIXME",
                      "number" .= lastEventId ship,
                      "author" .= Text.pack "~zod",
                      "when" .= Text.pack "FIXME", -- int(time.time() * 1000)
                      "letter" .= Aeson.object ["text" .= Text.pack "hello world!"]
                    ]
              ]
        ])


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
