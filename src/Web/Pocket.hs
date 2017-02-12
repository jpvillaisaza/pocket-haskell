{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Pocket
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Pocket
  ( AuthRequest (..)
  , makeAuthRequest
  , AuthorizeRequest (..)
  , makeAuthorizeRequest
  , authRequest
  , authorize
  , add
  , AddRequest (..)
  , makeAddRequest
  , AddResponse (..)
  , send
  , Action (..)
  , SendRequest (..)
  , SendResponse (..)
  , get
  , GetRequest (..)
  , makeGetRequest
  , GetResponse (..)
  , run
  , makeManager
  )
  where

-- base
import Data.Proxy (Proxy (..))

-- http-client
import Network.HTTP.Client hiding (Proxy)

-- http-client-tls
import Network.HTTP.Client.TLS

-- pocket
import Web.Pocket.Add
import Web.Pocket.Auth
import Web.Pocket.Get
import Web.Pocket.Send

-- servant
import Servant.API

-- servant-client
import Servant.Client hiding (Client)

-- transformers
import Control.Monad.IO.Class


-- |
--
--

type API =
  "v3"
    :>
      ( "oauth"
          :>
            ( "request"
                :> ReqBody '[JSON] AuthRequest
                :> Post '[JSON] AuthResponse
            :<|>
              "authorize"
                :> ReqBody '[JSON] AuthorizeRequest
                :> Post '[JSON] AuthorizeResponse
            )
      :<|>
        "add"
          :> ReqBody '[JSON] AddRequest
          :> Post '[JSON] AddResponse
      :<|>
        "send"
          :> ReqBody '[JSON] SendRequest
          :> Post '[JSON] SendResponse
      :<|>
        "get"
          :> ReqBody '[JSON] GetRequest
          :> Post '[JSON] GetResponse
      )


-- |
--
--

authRequest
  :: AuthRequest
  -> ClientM AuthResponse


-- |
--
--

authorize
  :: AuthorizeRequest
  -> ClientM AuthorizeResponse


-- |
--
--

add
  :: AddRequest
  -> ClientM AddResponse


-- |
--
--

send
  :: SendRequest
  -> ClientM SendResponse


-- |
--
--

get
  :: GetRequest
  -> ClientM GetResponse

(authRequest :<|> authorize) :<|> add :<|> send :<|> get =
  client (Proxy :: Proxy API)


-- |
--
--

run
  :: MonadIO m
  => Manager
  -> ClientM a
  -> m (Either ServantError a)
run manager =
  let
    baseUrl =
      BaseUrl
        Https
        "getpocket.com"
        443
        ""
  in
    liftIO . flip runClientM (ClientEnv manager baseUrl)


-- |
--
--

makeManager :: IO Manager
makeManager =
  let
    managerSettings =
      tlsManagerSettings
        { managerModifyRequest = \r ->
            return
              r
                { requestHeaders =
                    [ ("Content-Type", "application/json")
                    , ("X-Accept", "application/json")
                    ]
                }
        }
  in
    newManager managerSettings
