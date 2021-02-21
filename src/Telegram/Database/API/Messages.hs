{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS -fno-warn-partial-fields #-}
module Telegram.Database.API.Messages
  ( Message,
  )
where

import Data.Aeson
import Data.Aeson.Types
  ( FromJSON,
    ToJSON,
  )
import Data.Int
  ( Int32,
    Int64,
  )
import Data.Text as Text
import GHC.Generics (Generic)
import Telegram.Database.API.Text (FormattedText)
import qualified Telegram.Database.JSON as TDLib

data MessageSendingState
  = MessageSendingState -- TODO
  deriving (Show, Generic)

instance FromJSON MessageSendingState where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON MessageSendingState where
  toJSON = genericToJSON TDLib.jsonOptions

data MessageForwardInfo -- TODO
  = MessageForwardedFromUser
  | MessageForwardedPost
  deriving (Show, Generic)

instance FromJSON MessageForwardInfo where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON MessageForwardInfo where
  toJSON = genericToJSON TDLib.jsonOptions

data WebPage = WebPage
  {
  }
  deriving (Show, Generic) -- TODO

instance FromJSON WebPage where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON WebPage where
  toJSON = genericToJSON TDLib.jsonOptions

data MessageContent
  = MessageAnimation
  | MessageAudio
  | MessageBasicGroupChatCreate
  | MessageCall
  | MessageChatAddMembers
  | MessageChatChangePhoto
  | MessageChatChangeTitle
  | MessageChatDeleteMember
  | MessageChatDeletePhoto
  | MessageChatJoinByLink
  | MessageChatSetTtl
  | MessageChatUpgradeFrom
  | MessageChatUpgradeTo
  | MessageContact
  | MessageContactRegistered
  | MessageCustomServiceAction
  | MessageDocument
  | MessageExpiredPhoto
  | MessageExpiredVideo
  | MessageGame
  | MessageGameScore
  | MessageInvoice
  | MessageLocation
  | MessagePassportDataReceived
  | MessagePassportDataSent
  | MessagePaymentSuccessful
  | MessagePaymentSuccessfulBot
  | MessagePhoto
  | MessagePinMessage
  | MessageScreenshotTaken
  | MessageSticker
  | MessageSupergroupChatCreate
  | MessageText
      { text :: FormattedText, -- Text of the message.
        web_page :: Maybe WebPage -- A preview of the web page that's mentioned in the text; may be null.
      }
  | MessageUnsupported
  | MessageVenue
  | MessageVideo
  | MessageVideoNote
  | MessageVoiceNote
  | MessageWebsiteConnected
  deriving (Show, Generic)

instance FromJSON MessageContent where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON MessageContent where
  toJSON = genericToJSON TDLib.jsonOptions

data ReplyMarkup
  = ReplyMarkup
  deriving (Show, Generic) -- TODO

instance FromJSON ReplyMarkup where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON ReplyMarkup where
  toJSON = genericToJSON TDLib.jsonOptions

data Messages = Messages
  { total_count :: Int32, -- Approximate total count of messages found.
    messages :: [Maybe Message] -- List of messages; messages may be null.
  }
  deriving (Show, Generic)

instance FromJSON Messages where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON Messages where
  toJSON = genericToJSON TDLib.jsonOptions

newtype Int64String
  = Int64String Int64
  deriving (Show, Generic)

instance ToJSON Int64String where
  toJSON (Int64String int) = String . Text.pack . show $ (fromIntegral int :: Integer)
  {-# INLINE toJSON #-}

instance FromJSON Int64String where
  parseJSON = withText "Int64String" $ pure . Int64String . read . Text.unpack
  {-# INLINE parseJSON #-}

data Message = Message
  { id :: Int64, -- Message identifier, unique for the chat to which the message belongs.
    sender_user_id :: Int32, -- Identifier of the user who sent the message; 0 if unknown. It is unknown for channel posts.
    chat_id :: Int64, -- Chat identifier.
    sending_state :: Maybe MessageSendingState, -- Information about the sending state of the message; may be null.
    is_outgoing :: Bool, -- True, if the message is outgoing.
    can_be_edited :: Bool, -- True, if the message can be edited.
    can_be_forwarded :: Bool, -- True, if the message can be forwarded.
    can_be_deleted_only_for_self :: Bool, -- True, if the message can be deleted only for the current user while other users will continue to see it.
    can_be_deleted_for_all_users :: Bool, -- True, if the message can be deleted for all users.
    is_channel_post :: Bool, -- True, if the message is a channel post. All messages to channels are channel posts, all other messages are not channel posts.
    contains_unread_mention :: Bool, -- True, if the message contains an unread mention for the current user.
    date :: Int32, -- Point in time (Unix timestamp) when the message was sent.
    edit_date :: Int32, -- Point in time (Unix timestamp) when the message was last edited.
    forward_info :: Maybe MessageForwardInfo, -- Information about the initial message sender; may be null.
    reply_to_message_id :: Int64, -- If non-zero, the identifier of the message this message is replying to; can be the identifier of a deleted message.
    ttl :: Int32, -- For self-destructing messages, the message's TTL (Time To Live), in seconds; 0 if none. TDLib will send updateDeleteMessages or updateMessageContent once the TTL expires.
    ttl_expires_in :: Double, -- Time left before the message expires, in seconds.
    via_bot_user_id :: Int32, -- If non-zero, the user identifier of the bot through which this message was sent.
    author_signature :: String, -- For channel posts, optional author signature.
    views :: Int32, -- Number of times this message was viewed.
    media_album_id :: Int64String, -- Unique identifier of an album this message belongs to. Only photos and videos can be grouped together in albums.
    content :: MessageContent, -- Content of the message.
    reply_markup :: Maybe ReplyMarkup -- Reply markup for the message; may be null.
  }
  deriving (Show, Generic)

instance FromJSON Message where
  parseJSON = genericParseJSON TDLib.jsonOptions

instance ToJSON Message where
  toJSON = genericToJSON TDLib.jsonOptions

-- forwardMessages :: Int64 -> Int64 -> [Int64] -> Bool -> Bool -> Bool -> Client -> IO (Maybe Messages)
-- forwardMessages chatId fromChatId messageIds disableNotification fromBackground asAlbum =
--   TDLib.execute $ object [
--     ("@type", String "viewMessages"),
--     ("chat_id", Number (scientific chatId 0)),
--     ("message_ids", Array $ fromList [Number (scientific msgId 0)]),
--     ("force_read", Bool True)
--   ]
