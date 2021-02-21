module Main
  ( main
  )
where

import           Telegram.Client
import           Telegram.Database.JSON        as TDLib

main :: IO ()
main = do
  client <- TDLib.create
  -- TDLib.sendJSON "{\"@type\": \"getAuthorizationState\"}" client
  -- TDLib.receiveJSON client >>= print
  process defaultHandler client
  TDLib.destroy client
