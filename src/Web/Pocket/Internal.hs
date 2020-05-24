{-# LANGUAGE OverloadedStrings #-}

module Web.Pocket.Internal where

-- aeson
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson

-- base
import Control.Monad.IO.Class (MonadIO)

-- exceptions
import Control.Monad.Catch (MonadThrow)

-- http-conduit
import Network.HTTP.Simple


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
