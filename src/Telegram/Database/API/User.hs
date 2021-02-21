{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Telegram.Database.API.User
  ( User
  )
where

import qualified Telegram.Database.JSON        as TDLib
import           GHC.Generics                   ( Generic )
import           Data.Aeson
import           Data.Aeson.Types               ( FromJSON, ToJSON)
import Data.Int (Int32)

data User = User {
  id :: Int32,
  first_name :: String,
  last_name :: String,
  username :: String
} deriving (Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON User where
  toJSON = genericToJSON TDLib.jsonOptions

