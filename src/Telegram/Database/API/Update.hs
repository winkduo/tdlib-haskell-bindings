{-# LANGUAGE DeriveGeneric #-}

module Telegram.Database.API.Update where

import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Telegram.Database.API.Authorization
import           Telegram.Database.API.Messages
import           Telegram.Database.JSON        as TDLib

data Update = 
    UpdateAuthorizationState {
      authorization_state :: AuthorizationState
    }
  | UpdateBasicGroup
  | UpdateBasicGroupFullInfo
  | UpdateCall
  | UpdateChatDefaultDisableNotification
  | UpdateChatDraftMessage
  | UpdateChatIsMarkedAsUnread
  | UpdateChatIsPinned
  | UpdateChatIsSponsored
  | UpdateChatLastMessage
  | UpdateChatNotificationSettings
  | UpdateChatOrder
  | UpdateChatPhoto
  | UpdateChatReadInbox
  | UpdateChatReadOutbox
  | UpdateChatReplyMarkup
  | UpdateChatTitle
  | UpdateChatUnreadMentionCount
  | UpdateConnectionState
  | UpdateDeleteMessages
  | UpdateFavoriteStickers
  | UpdateFile
  | UpdateFileGenerationStart
  | UpdateFileGenerationStop
  | UpdateInstalledStickerSets
  | UpdateLanguagePackStrings
  | UpdateMessageContent
  | UpdateMessageContentOpened
  | UpdateMessageEdited
  | UpdateMessageMentionRead
  | UpdateMessageSendAcknowledged
  | UpdateMessageSendFailed
  | UpdateMessageSendSucceeded
  | UpdateMessageViews
  | UpdateNewCallbackQuery
  | UpdateNewChat
  | UpdateNewChosenInlineResult
  | UpdateNewCustomEvent
  | UpdateNewCustomQuery
  | UpdateNewInlineCallbackQuery
  | UpdateNewInlineQuery
  | UpdateNewMessage {
      message :: Message,
      disable_notification :: Bool,
      contains_mention :: Bool
    }
  | UpdateNewPreCheckoutQuery
  | UpdateNewShippingQuery
  | UpdateNotificationSettings
  | UpdateOption
  | UpdateRecentStickers
  | UpdateSavedAnimations
  | UpdateScopeNotificationSettings
  | UpdateSecretChat
  | UpdateServiceNotification
  | UpdateSupergroup
  | UpdateSupergroupFullInfo
  | UpdateTermsOfService
  | UpdateTrendingStickerSets
  | UpdateUnreadChatCount
  | UpdateUnreadMessageCount
  | UpdateUser
  | UpdateUserChatAction
  | UpdateUserFullInfo
  | UpdateUserPrivacySettingRules
  | UpdateUserStatus
  | Ok
    deriving (Show, Generic)

instance FromJSON Update where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON Update where
  toJSON = genericToJSON TDLib.jsonOptions
