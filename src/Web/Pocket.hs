{-# LANGUAGE OverloadedStrings #-}

module Web.Pocket
  ( authRequest
  , authAuthorize
  , makeRedirect
  , add
  , get
  , send
  )
  where

-- base
import Control.Monad.IO.Class (MonadIO)

-- exceptions
import Control.Monad.Catch (MonadThrow)

-- pocket
import Web.Pocket.Add
import Web.Pocket.Auth
import Web.Pocket.Error
import Web.Pocket.Get
import Web.Pocket.Request
import Web.Pocket.Send

-- text
import Data.Text (Text)


authRequest
  :: (MonadIO m, MonadThrow m)
  => AuthRequestReq
  -> m (Either Error AuthRequestRsp)
authRequest =
  request "POST https://getpocket.com/v3/oauth/request"

authAuthorize
  :: (MonadIO m, MonadThrow m)
  => AuthAuthorizeReq
  -> m (Either Error AuthAuthorizeRsp)
authAuthorize =
  request "POST https://getpocket.com/v3/oauth/authorize"

makeRedirect :: AuthRequestReq -> AuthRequestRsp -> Text
makeRedirect req rsp =
  "https://getpocket.com/auth/authorize?request_token="
    <> authRequestRspCode rsp
    <> "&redirect_uri="
    <> authRequestReqRedirectUri req

add
  :: (MonadIO m, MonadThrow m)
  => AddReq
  -> m (Either Error AddRsp)
add =
  request "POST https://getpocket.com/v3/add"

get
  :: (MonadIO m, MonadThrow m)
  => GetReq
  -> m (Either Error GetRsp)
get =
  request "POST https://getpocket.com/v3/get"

send
  :: (MonadIO m, MonadThrow m)
  => SendReq
  -> m (Either Error SendRsp)
send =
  request "POST https://getpocket.com/v3/send"
