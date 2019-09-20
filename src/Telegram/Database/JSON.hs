{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module Telegram.Database.JSON
  ( Client
  , create
  , sendJSON
  , receiveJSON
  , receiveJSONWithTimeout
  , executeJSON
  , send
  , receive
  , receiveWithTimeout
  , execute
  , receiveEither
  , receiveEitherWithTimeout
  , executeEither
  , destroy
  , jsonOptions
  )
where

import           Data.Aeson
import           Data.ByteString                ( ByteString
                                                , packCString
                                                , useAsCString
                                                )
import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                )
import           Data.Char                     as Char
import           Foreign                        ( Ptr
                                                , nullPtr
                                                )
import           Foreign.C.String               ( CString )
import           Foreign.C.Types                ( CDouble(CDouble) )

type Client   = Ptr ()
type Request  = CString
type Response = CString
type Timeout  = CDouble

foreign import ccall "libtdjson td_json_client_create"  c_create  :: IO Client
foreign import ccall "libtdjson td_json_client_send"    c_send    :: Client -> Request -> IO ()
foreign import ccall "libtdjson td_json_client_receive" c_receive :: Client -> Timeout -> IO Response
foreign import ccall "libtdjson td_json_client_execute" c_execute :: Client -> Request -> IO Response
foreign import ccall "libtdjson td_json_client_destroy" c_destroy :: Client -> IO ()

create :: IO Client
create = c_create

sendJSON :: ByteString -> Client -> IO ()
sendJSON request client = useAsCString request $ c_send client

receiveJSON :: Client -> IO (Maybe ByteString)
receiveJSON = receiveJSONWithTimeout 1.0

receiveJSONWithTimeout :: Double -> Client -> IO (Maybe ByteString)
receiveJSONWithTimeout timeout client =
  c_receive client (realToFrac timeout) >>= safeCString

executeJSON :: ByteString -> Client -> IO (Maybe ByteString)
executeJSON request client =
  useAsCString request (c_execute client) >>= safeCString

send :: ToJSON a => a -> Client -> IO ()
send = sendJSON . toStrict . encode

receive :: FromJSON a => Client -> IO (Maybe a)
receive = receiveWithTimeout 1.0

receiveWithTimeout :: FromJSON a => Double -> Client -> IO (Maybe a)
receiveWithTimeout timeout client =
  (decodeStrict =<<) <$> receiveJSONWithTimeout timeout client

execute :: (ToJSON a, FromJSON b) => a -> Client -> IO (Maybe b)
execute request client =
  (decodeStrict =<<) <$> (executeJSON . toStrict . encode) request client

receiveEither :: FromJSON a => Client -> IO (Either String a)
receiveEither = receiveEitherWithTimeout 1.0

receiveEitherWithTimeout :: FromJSON a => Double -> Client -> IO (Either String a)
receiveEitherWithTimeout timeout client =
  maybe (Left "NULL") eitherDecodeStrict <$> receiveJSONWithTimeout timeout client

executeEither :: (ToJSON a, FromJSON b) => a -> Client -> IO (Either String b)
executeEither request client =
  maybe (Left "NULL") eitherDecodeStrict <$> (executeJSON . toStrict . encode) request client

destroy :: Client -> IO ()
destroy = c_destroy

safeCString :: CString -> IO (Maybe ByteString)
safeCString str
  | str == nullPtr = return Nothing
  | otherwise      = Just <$> packCString str

jsonOptions :: Options
jsonOptions = defaultOptions {
  sumEncoding = TaggedObject {
    tagFieldName = "@type",
    contentsFieldName = ""
  },
  tagSingleConstructors = True,
  omitNothingFields = True,
  constructorTagModifier = \case
    "" -> ""
    x : xs -> Char.toLower x : xs
}
