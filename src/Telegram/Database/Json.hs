{-# LANGUAGE ForeignFunctionInterface #-}

module Telegram.Database.Json (Client, create, send, receive, execute, destroy) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Control.Monad (liftM)

type Client = Ptr ()
type Request = CString
type Response = CString
type Timeout = CDouble

foreign import ccall "libtdjson td_json_client_create" c_create :: IO Client
foreign import ccall "libtdjson td_json_client_send" c_send :: Client -> Request -> IO ()
foreign import ccall "libtdjson td_json_client_receive" c_receive :: Client -> Timeout -> IO Response
foreign import ccall "libtdjson td_json_client_execute" c_execute :: Client -> Request -> IO Response
foreign import ccall "libtdjson td_json_client_destroy" c_destroy :: Client -> IO ()

create :: IO Client
create = c_create

send :: Client -> String -> IO ()
send client request = newCString request >>= c_send client

receive :: Client -> IO (Maybe String)
receive client = c_receive client 1.0 >>= safeCString

execute :: Client -> String -> IO (Maybe String)
execute client request = newCString request >>= c_execute client >>= safeCString

destroy :: Client -> IO ()
destroy = c_destroy

safeCString :: CString -> IO (Maybe String)
safeCString str
  | str == nullPtr = return Nothing
  | otherwise = Just <$> peekCString str
