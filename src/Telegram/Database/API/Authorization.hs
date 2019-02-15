{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Telegram.Database.API.Authorization where

import           Data.Aeson
import           Data.Aeson.Types               ( ToJSON, FromJSON )
import qualified Data.Char                     as Char
import           Data.Int
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Telegram.Database.JSON        as TDLib
import           Telegram.Database.JSON         ( Client )

data TermsOfService =  -- TODO
  TermsOfService {
    min_user_age :: Int32
  }
  deriving (Show, Generic)

instance FromJSON TermsOfService where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON TermsOfService where
  toJSON = genericToJSON TDLib.jsonOptions

data AuthenticationCodeInfo =  -- TODO
  AuthenticationCodeInfo {
    phone_number :: String
  }
  deriving (Show, Generic)

instance FromJSON AuthenticationCodeInfo where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON AuthenticationCodeInfo where
  toJSON = genericToJSON TDLib.jsonOptions

data AuthorizationState =
    AuthorizationStateClosed
  | AuthorizationStateClosing
  | AuthorizationStateLoggingOut
  | AuthorizationStateReady
  | AuthorizationStateWaitCode {
      is_registered    :: Bool,
      terms_of_service :: Maybe TermsOfService,
      code_info        :: AuthenticationCodeInfo
    }
  | AuthorizationStateWaitEncryptionKey{
      is_encrypted     :: Bool
    }
  | AuthorizationStateWaitPassword {
      password_hint                  :: String,
      has_recovery_email_address     :: Bool,
      recovery_email_address_pattern :: String
    }
  | AuthorizationStateWaitPhoneNumber
  | AuthorizationStateWaitTdlibParameters
  deriving (Show, Generic)

instance FromJSON AuthorizationState where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON AuthorizationState where
  toJSON = genericToJSON TDLib.jsonOptions

data TdlibParameters = TdlibParameters {
  use_test_dc              :: Bool,
  database_directory       :: String,
  files_directory          :: String,
  use_file_database        :: Bool,
  use_chat_info_database   :: Bool,
  use_message_database     :: Bool,
  use_secret_chats         :: Bool,
  api_id                   :: Int,
  api_hash                 :: String,
  system_language_code     :: String,
  device_model             :: String,
  system_version           :: String,
  application_version      :: String,
  enable_storage_optimizer :: Bool,
  ignore_file_names        :: Bool
} deriving (Generic, ToJSON, FromJSON)

defaultTdlibParameters :: TdlibParameters
defaultTdlibParameters = TdlibParameters {
  use_test_dc              = False,
  database_directory       = "tdlib",
  files_directory          = "",
  use_file_database        = True,
  use_chat_info_database   = True,
  use_message_database     = True,
  use_secret_chats         = True,
  api_id                   = undefined,
  api_hash                 = undefined,
  system_language_code     = "en",
  device_model             = "Desktop",
  system_version           = "Unknown",
  application_version      = "0.1.0",
  enable_storage_optimizer = True,
  ignore_file_names        = False
}

setTdlibParameters :: TdlibParameters -> Client -> IO ()
setTdlibParameters parameters = TDLib.send $ object [
    "@type"      .= String "setTdlibParameters",
    "parameters" .= parameters
  ]

checkDatabaseEncryptionKey :: Text -> Client -> IO ()
checkDatabaseEncryptionKey encryptionKey = TDLib.send $ object [
    "@type"          .= String "checkDatabaseEncryptionKey",
    "encryption_key" .= String encryptionKey
  ]

setAuthenticationPhoneNumber :: Text -> Bool -> Bool -> Client -> IO ()
setAuthenticationPhoneNumber phoneNumber allowFlashCall isCurrentPhoneNumber =
  TDLib.send $ object [
    "@type"                   .= String "setAuthenticationPhoneNumber",
    "phone_number"            .= String phoneNumber,
    "allow_flash_call"        .= Bool allowFlashCall,
    "is_current_phone_number" .= Bool isCurrentPhoneNumber
  ]
    
checkAuthenticationCode :: Text -> Text -> Text -> Client -> IO ()
checkAuthenticationCode code firstName lastName = TDLib.send $ object [
    "@type"      .= String "checkAuthenticationCode",
    "code"       .= String code,
    "first_name" .= String firstName,
    "last_name"  .= String lastName
  ]

checkAuthenticationPassword :: Text -> Client -> IO ()
checkAuthenticationPassword password = TDLib.send $ object [
    "@type"    .= String "checkAuthenticationPassword",
    "password" .= String password
  ]
