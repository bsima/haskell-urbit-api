{-# LANGUAGE OverloadedStrings #-}

module Main where

import Urbit.Airlock

main = do
  let ship = Ship {
    session = Nothing,
    lastEventId = 0,
    url = "http://localhost:8081",
    code = "lidlut-tabwed-pillex-ridrup",
    sseClient = False
  }
  connect ship
  putStrLn "success"
