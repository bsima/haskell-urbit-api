# Haskell Urbit API

[![License MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
[![Hackage](https://img.shields.io/hackage/v/urbit-api.svg?style=flat)](https://hackage.haskell.org/package/urbit-api)
[![builds.sr.ht status](https://builds.sr.ht/~ben/urbit-api.svg)](https://builds.sr.ht/~ben/urbit-api?)



This library helps you talk to your Urbit from Haskell, via HTTP.

The Urbit API is a command-query API that lets you hook into apps running on
your Urbit. You can submit commands (called "pokes") and subscribe to
responses.

See the `test.hs` file for some example usages.

## Design

The Urbit vane `eyre` is responsible for defining the API interface. The path to
the API is `/~/channel/...`, where we send messages to the global log (called
`poke`s) which are then dispatched to the appropriate apps. To receive
responses, we stream messages from a path associated with the app, such as
`/mailbox/~/~zod/mc`. Internally, I believe Urbit calls these `wire`s.

`urbit-api` handles most of the path, session, and HTTP request stuff
automatically. See the
[haddocks](https://hackage.haskell.org/package/urbit-api/docs/Urbit-API.html)
for more details.

This library is built on req, conduit, and aeson, all of which are very stable
and usable libraries for working with HTTP requests and web data.

## Example usage

```haskell
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Text as Text
import qualified Data.UUID.V4 as UUID

import Urbit.API

main :: IO ()
main = do
  let fakezod = Ship
    { uid = "0123456789abcdef",
      name = "zod",
      lastEventId = 1,
      url = "http://localhost:8081",
      code = "lidlut-tabwed-pillex-ridrup"
    }

  -- Establish connection
  sess <- connect ship

  -- Send a message by poking the chat-hook
  uuid <- UUID.nextRandom
  poke sess ship "zod" "chat-hook" "json" $
    Aeson.object
      [ "message"
          .= Aeson.object
            [ "path" .= Text.pack "/~/~zod/mc",
              "envelope"
                .= Aeson.object
                  [ "uid" .= UUID.toText uuid,
                    "number" .= (1 :: Int),
                    "author" .= Text.pack "~zod",
                    "when" .= (1602118786225 :: Int),
                    "letter" .= Aeson.object ["text" .= Text.pack "hello world from haskell!"]
                  ]
            ]
      ]
```

## TODO

- fix test suite on travis (OOM when trying to compile urbit)
- more sophisticated test cases, also use cabal test instead of homegrown thing
- add an exe that wraps the library with a cli
- port to ghcjs
- put some examples in the docs
- graph store interface
- additional agent interfaces
