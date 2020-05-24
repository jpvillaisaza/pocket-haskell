{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Pocket
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Pocket
  where

-- aeson
import qualified Data.Aeson as Aeson

-- base
import Control.Monad.IO.Class (MonadIO)

-- http-conduit
import Network.HTTP.Simple

-- pocket
import Web.Pocket.Add
import Web.Pocket.Auth
import Web.Pocket.Get
import Web.Pocket.Send

-- text
import qualified Data.Text as Text


authReq :: AuthRequest -> IO AuthResponse
authReq ar = do
  request' <- parseRequest "POST https://getpocket.com/v3/oauth/request"
  let
    request =
      setRequestBodyJSON ar
        $ setRequestHeaders [ ("Content-Type", "application/json")
        , ("X-Accept", "application/json")
        ]
        $ request'
  response <- httpLBS request
  case Aeson.decode (getResponseBody response) of
    Just authResponse -> pure authResponse
    Nothing -> undefined

authGet :: AuthRequest -> IO String
authGet ar = do
  authRsp <- authReq ar
  pure $
    "https://getpocket.com/auth/authorize?request_token="
      <> Text.unpack (authRespCode authRsp)
      <> "&redirect_uri="
      <> Text.unpack (authReqRedirectUri ar)

authorizeReq :: AuthorizeRequest -> IO AuthorizeResponse
authorizeReq ar = do
  request' <- parseRequest "POST https://getpocket.com/v3/oauth/authorize"
  let
    request =
      setRequestBodyJSON ar
        $ setRequestHeaders [ ("Content-Type", "application/json")
        , ("X-Accept", "application/json")
        ]
        $ request'
  response <- httpLBS request
  print response
  case Aeson.eitherDecode (getResponseBody response) of
    Right authResponse -> pure authResponse
    Left e -> fail e


add :: AddRequest -> IO AddResponse
add addReq = do
  request' <- parseRequest "POST https://getpocket.com/v3/add"
  let
    request =
      setRequestBodyJSON addReq
        $ setRequestHeaders [ ("Content-Type", "application/json")
        , ("X-Accept", "application/json")
        ]
        $ request'
  response <- httpLBS request
  case Aeson.decode (getResponseBody response) of
    Just authResponse -> pure authResponse
    Nothing -> do
      let headers = getResponseHeader "X-Error" response
      undefined

send :: MonadIO m => SendRequest -> m SendResponse
send =
  -- "POST https://getpocket.com/v3/send"
  undefined

get :: MonadIO m => GetRequest -> m GetResponse
get =
  -- "POST https://getpocket.com/v3/GET"
  undefined
