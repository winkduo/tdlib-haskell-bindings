module Main where

import Telegram.Database.Json as TDLib

main :: IO ()
main = do
  client <- TDLib.create
  TDLib.send client "{\"@type\": \"getAuthorizationState\", \"@extra\": 1.01234}"
  TDLib.receive client >>= print
  TDLib.destroy client
