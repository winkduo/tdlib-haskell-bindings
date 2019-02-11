{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Telegram.Database.JSON        as TDLib

main :: IO ()
main = do
  client <- TDLib.create
  TDLib.sendJSON "{\"@type\": \"getAuthorizationState\"}" client
  TDLib.receiveJSON client >>= print
  TDLib.destroy client
