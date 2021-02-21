{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Telegram.Database.API.Text
  ( FormattedText
  )
where

import           Data.Aeson
import           Data.Aeson.Types               ( FromJSON
                                                , ToJSON 
                                                )

import           GHC.Generics                   ( Generic )

import qualified Telegram.Database.JSON        as TDLib

data FormattedText = 
  FormattedText {
    text :: String
    -- entities :: []
  }
  deriving (Show, Generic) -- TODO

instance FromJSON FormattedText where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON FormattedText where
  toJSON = genericToJSON TDLib.jsonOptions
