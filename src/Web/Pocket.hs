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
  ( authReq
  , authGet
  , authorizeReq
  , add
  , get
  , send
  )
  where

-- aeson
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson

-- base
import Control.Monad.IO.Class (MonadIO)

-- exceptions
import Control.Monad.Catch (MonadThrow)

-- http-conduit
import Network.HTTP.Simple

-- pocket
import Web.Pocket.Add
import Web.Pocket.Auth
import Web.Pocket.Get
import Web.Pocket.Send

-- text
import qualified Data.Text as Text


authReq :: (MonadIO m, MonadThrow m) => AuthRequest -> m AuthResponse
authReq =
  common "POST https://getpocket.com/v3/oauth/request"

authGet :: AuthRequest -> IO String
authGet ar = do
  authRsp <- authReq ar
  pure $
    "https://getpocket.com/auth/authorize?request_token="
      <> Text.unpack (authRespCode authRsp)
      <> "&redirect_uri="
      <> Text.unpack (authReqRedirectUri ar)

authorizeReq :: (MonadIO m, MonadThrow m) => AuthorizeRequest -> m AuthorizeResponse
authorizeReq =
  common "POST https://getpocket.com/v3/oauth/authorize"


add :: (MonadIO m, MonadThrow m) => AddRequest -> m AddResponse
add =
  common "POST https://getpocket.com/v3/add"

get :: (MonadIO m, MonadThrow m) => GetRequest -> m GetResponse
get =
  common "POST https://getpocket.com/v3/get"

send :: (MonadIO m, MonadThrow m) => SendRequest -> m SendResponse
send =
  common "POST https://getpocket.com/v3/send"

common :: (ToJSON a, FromJSON b, MonadIO m, MonadThrow m) => String -> a -> m b
common req r = do
  request' <- parseRequest req
  let
    request =
      setRequestBodyJSON r
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
