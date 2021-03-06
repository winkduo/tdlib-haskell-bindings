{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS -fno-warn-partial-fields #-}

module Telegram.Database.API.Update
  ( Update (..),
  )
where

import Data.Aeson
import GHC.Generics
import Telegram.Database.API.Authorization
import Telegram.Database.API.Messages
import Telegram.Database.API.User
import Telegram.Database.JSON as TDLib

data Update
  = UpdateAuthorizationState
      { authorization_state :: AuthorizationState
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
  | UpdateNewMessage
      { message :: Message,
        disable_notification :: Maybe Bool,
        contains_mention :: Maybe Bool
      }
  | UpdateNewPreCheckoutQuery
  | UpdateNewShippingQuery
  | UpdateNotificationSettings
  | UpdateOption
  | UpdateRecentStickers
  | UpdateSavedAnimations
  | UpdateScopeNotificationSettings
  | UpdateSecretChat
  | UpdateSelectedBackground
  | UpdateServiceNotification
  | UpdateSupergroup
  | UpdateSupergroupFullInfo
  | UpdateTermsOfService
  | UpdateTrendingStickerSets
  | UpdateUnreadChatCount
  | UpdateUnreadMessageCount
  | UpdateUser
      { user :: User
      }
  | UpdateUserChatAction
  | UpdateUserFullInfo
  | UpdateUserPrivacySettingRules
  | UpdateUserStatus
  | UpdateHavePendingNotifications
  | Ok
  deriving (Show, Generic)

instance FromJSON Update where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON Update where
  toJSON = genericToJSON TDLib.jsonOptions
