{-# LANGUAGE OverloadedStrings #-}

module Web.Pocket.Request
  ( request
  )
  where

-- aeson
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson

-- base
import Control.Monad.IO.Class (MonadIO)

-- bytestring
import Data.ByteString (ByteString)

-- exceptions
import Control.Monad.Catch (MonadThrow)

-- http-conduit
import Network.HTTP.Simple

-- http-types
import Network.HTTP.Types (HeaderName)

-- pocket
import Web.Pocket.Error (Error)
import qualified Web.Pocket.Error as Error


request
  :: (MonadIO m, MonadThrow m, ToJSON a, FromJSON b)
  => String
  -> a
  -> m (Either Error b)
request req r = do
  let
    setRequest =
      setRequestBodyJSON r . setRequestHeaders headers
  response <- httpLBS =<< fmap setRequest (parseRequest req)
  case Aeson.decode (getResponseBody response) of
    Just authResponse ->
      pure (Right authResponse)
    Nothing ->
      pure (Left (Error.fromResponse response))

headers :: [(HeaderName, ByteString)]
headers =
  [ ("Content-Type", "application/json")
  , ("X-Accept", "application/json")
  ]
