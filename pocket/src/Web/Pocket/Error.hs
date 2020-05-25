{-# LANGUAGE OverloadedStrings #-}

module Web.Pocket.Error
  ( Error(..)
  , fromResponse
  )
  where

-- bytestring
import Data.ByteString (ByteString)

-- http-conduit
import Network.HTTP.Simple

-- http-types
import Network.HTTP.Types


data Error =
  Error
    { errorStatusCode :: Int
    , errorStatusMessage :: ByteString
    , errorXError :: Maybe ByteString
    , errorXErrorCode :: Maybe ByteString
    , errorXLimitUserLimit :: Maybe ByteString
    , errorXLimitUserRemaining :: Maybe ByteString
    , errorXLimitUserReset :: Maybe ByteString
    , errorXLimitKeyLimit :: Maybe ByteString
    , errorXLimitKeyRemaining :: Maybe ByteString
    , errorXLimitKeyReset :: Maybe ByteString
    }
  deriving (Show)

fromResponse :: Response a -> Error
fromResponse response =
  let
    headers =
      getResponseHeaders response
  in
    Error
      { errorStatusCode = getResponseStatusCode response
      , errorStatusMessage = statusMessage (getResponseStatus response)
      , errorXError = lookup "X-Error" headers
      , errorXErrorCode = lookup "X-Error-Code" headers
      , errorXLimitUserLimit = lookup "X-Limit-User-Limit" headers
      , errorXLimitUserRemaining = lookup "X-Limit-User-Remaining" headers
      , errorXLimitUserReset = lookup "X-Limit-User-Reset" headers
      , errorXLimitKeyLimit = lookup "X-Limit-Key-Limit" headers
      , errorXLimitKeyRemaining = lookup "X-Limit-Key-Remaining" headers
      , errorXLimitKeyReset = lookup "X-Limit-Key-Reset" headers
      }
