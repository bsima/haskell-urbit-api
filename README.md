# Haskell Urbit API

[![License MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
[![Hackage](https://img.shields.io/hackage/v/urbit-airlock.svg?style=flat)](https://hackage.haskell.org/package/urbit-airlock)


This library helps you talk to your Urbit from Haskell, via HTTP.

The "Urbit Airlock" API is a command-query API that lets you hook into apps
running on your Urbit. You can submit commands (called "pokes") and subscribe to
responses.

See the `test.hs` file for some example usages.

## Design

The Urbit vane `eyre` is responsible for defining the API interface. The path to
the API is `/~/channel/...`, where we send messages to the global log (called
`poke`s) which are then dispatched to the appropriate apps. To receive
responses, we stream messages from a path associated with the app, such as
`/mailbox/~/~zod/mc`. Internally, I believe Urbit calls these `wire`s.

`urbit-airlock` handles most of the path, session, and HTTP request stuff
automatically. See the
[haddocks](https://hackage.haskell.org/package/urbit-airlock/docs/Urbit-Airlock.html)
for more details.

This library is built on req, conduit, and aeson, all of which are very stable
and usable libraries for working with HTTP requests and web data.

## TODO

- fix test suite on travis (OOM when trying to compile urbit)
- more sophisticated test cases, also use cabal test instead of homegrown thing
- add an exe that wraps the library with a cli
- port to ghcjs
- put some examples in the docs
