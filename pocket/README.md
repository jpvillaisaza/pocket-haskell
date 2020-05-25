# Haskell bindings for the Pocket API

See [Pocket][pocket] and the [Pocket API][pocket-api].

## Usage

### Authentication

See [Pocket API: Authentication][pocket-api-auth].

#### Example

1. Go to your [Pocket applications][pocket-api-apps] and get a consumer key:

   ```haskell
   let consumerKey = "1234-abcd1234abcd1234abcd1234"
   ```

2. Obtain a request token from Pocket:

   ```haskell
   let
     authRequestReq =
       AuthRequestReq "<consumerKey>" "<redirectUri>" Nothing
   authRequest authRequestReq
   ```

3. Redirect the user to Pocket:

   ```haskell
   makeRedirect authRequestReq authRequestRsp
   ```

4. Receive the callback from Pocket.

5. Convert the request token into an access token:

   ```haskell
   let
     authAuthorizeReq =
       AuthAuthorizeReq "<consumerKey>" "<requestToken>"
   authAuthorize authAuthorizeReq
   ```

### Add

See [Pocket API: Add][pocket-api-add].

#### Example

Use your application’s consumer key and a user’s access token to add items to
Pocket:

```haskell
let
  addReq =
    makeAddReq "<consumerKey>" "<accessToken>" "<url>"
add addReq
```

### Modify

See [Pocket API: Modify][pocket-api-send].

TODO

### Retrieve

See [Pocket API: Retrieve][pocket-api-get].

#### Example

Use your application’s consumer key and a user’s access token to retrieve data
from Pocket:

```haskell
let
  getReq =
    (makeGetReq "<consumerKey>" "<accessToken>")
      { getReqCount = Just 1
      , getReqSort = Just "newest"
      }
get getReq
```

[pocket]: https://getpocket.com
[pocket-api]: https://getpocket.com/developer/
[pocket-api-apps]: https://getpocket.com/developer/apps/
[pocket-api-add]: https://getpocket.com/developer/docs/v3/add
[pocket-api-auth]: https://getpocket.com/developer/docs/authentication
[pocket-api-get]: https://getpocket.com/developer/docs/v3/retrieve
[pocket-api-send]: https://getpocket.com/developer/docs/v3/modify
