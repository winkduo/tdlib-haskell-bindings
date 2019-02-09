{-# LANGUAGE ForeignFunctionInterface #-}

module Telegram.Database.Json (Client, create, send, receive, receiveWithTimeout, execute, destroy) where

import Data.ByteString (ByteString, packCString, useAsCString)

import Foreign
import Foreign.C.String
import Foreign.C.Types

type Client = Ptr ()
type Request = CString
type Response = CString
type Timeout = CDouble

foreign import ccall "libtdjson td_json_client_create"  c_create  :: IO Client
foreign import ccall "libtdjson td_json_client_send"    c_send    :: Client -> Request -> IO ()
foreign import ccall "libtdjson td_json_client_receive" c_receive :: Client -> Timeout -> IO Response
foreign import ccall "libtdjson td_json_client_execute" c_execute :: Client -> Request -> IO Response
foreign import ccall "libtdjson td_json_client_destroy" c_destroy :: Client -> IO ()

create :: IO Client
create = c_create

send :: Client -> ByteString -> IO ()
send client request = useAsCString request $ c_send client

receive :: Client -> IO (Maybe ByteString)
receive = receiveWithTimeout 1.0

receiveWithTimeout :: Double -> Client -> IO (Maybe ByteString)
receiveWithTimeout timeout client = c_receive client (realToFrac timeout) >>= safeCString

execute :: Client -> ByteString -> IO (Maybe ByteString)
execute client request = useAsCString request (c_execute client) >>= safeCString

destroy :: Client -> IO ()
destroy = c_destroy

safeCString :: CString -> IO (Maybe ByteString)
safeCString str
  | str == nullPtr = return Nothing
  | otherwise = Just <$> packCString str
