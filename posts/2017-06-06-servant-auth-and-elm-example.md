---
title: Servant Auth and Elm
---

[servant-auth-and-elm-example](https://github.com/mchaver/servant-auth-and-elm-example)
is a simple project that shows you how to make an Elm application that can 
log in, log out and make authenticated requests to a servant back-end. 
servant-auth adds JSON Web Token authentication to servant. It supports 
authentication via API tokens or browser cookies. In this post I will highlight
how to authentication works between servant, servant-auth and Elm. Hopefully 
that will be sufficient for you to follow the code in the git repository.

## servant and servant-auth

The back-end code follows the example in the servant-auth [README.md](https://github.com/plow-technologies/servant-auth#how-it-works)
closely. The code is all in one file [Server.hs](https://github.com/mchaver/servant-auth-and-elm-example/blob/master/src/Server.hs).

#### Protected API

The `Protected` API requires authentication to use. Any resource that 
requires authentication should be placed in this API.

```haskell
data User =
  User
    { userId       :: Int
    , userEmail    :: Text 
    , userPassword :: Text
    } deriving (Eq, Read, Show, Generic)

data Login = 
  Login 
    { username :: Text
    , password :: Text
    } deriving (Eq, Read, Show, Generic)

type Protected = 
       "die"      :> "roll" :> Get '[JSON] Int
  :<|> "loggedin" :> Get '[JSON] User
  
protected :: AuthResult User -> Server Protected
protected (Authenticated _user) = 
       (liftIO $ randomRIO (1, 6)) 
  :<|> (return $ User 1 "test@test.com" "")
protected _ = throwAll err401
```

The first route is just a simple random number generator for the front-end to 
call when the user is logged in. The front-end uses the second route to see if
the current token in the browser's cookies are valid. If they are then the 
browser gets the user data (excluding the password) and can display the user
email. You can expand this to include any user you want to display to the 
currently logged in user. For simplicity the `loggedin` route is just returning 
a hard coded value. This can easily be expanded to a database system of your 
choice.

#### Unprotected API

The `Unprotected` API provides any resources that do not require authentication:
login request, front-end files, home page, static resources, etc.

```haskell
type Unprotected = 
       "login" :> ReqBody '[JSON] Login
          :> PostNoContent '[JSON] 
              (Headers '[ Header "Set-Cookie" SetCookie
                       ,  Header "Set-Cookie" SetCookie]
                NoContent)
  :<|> Raw
```

The login route returns two Set-Cookie values in the response header if the 
authentication is successful and returns no value in the body. The last route 
`Raw` will catch all other API calls. Servant route lookup is in order of 
declaration, from top to bottom. `Raw` with no sub-paths needs to be located 
at the end or it will catch routes that we do not want it to catch. 

```haskell
checkCreds :: CookieSettings
  -> JWTSettings
  -> Login
  -> Handler 
      (Headers 
        '[ Header "Set-Cookie" SetCookie
         , Header "Set-Cookie" SetCookie]
       NoContent)
checkCreds 
  cookieSettings 
  jwtSettings 
  (Login loginUserIdent loginUserPassword) = do
  case mUser of
    Nothing -> throwError err401
    Just usr -> do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
      case mApplyCookies of
        Nothing     -> throwError err401
        Just applyCookies -> do 
          return $ applyCookies NoContent
  where
    mUser =
      if loginUserIdent == "test@test.com" && loginUserPassword == "password"
        then Just $ User 1 "test@test.com" "test"
        else Nothing
```

`checkCreds` handles authentication. Again, for simplicity we use hard coded 
values but we can easily add a database to handle user data.

```haskell
staticFiles :: [(FilePath, ByteString)]
staticFiles =
  [ ("index.html", $(embedFile "static/index.html"))
  ]

unprotected :: CookieSettings 
            -> JWTSettings 
            -> Server Unprotected
unprotected cs jwts = 
  loginH :<|> staticH
  where
    loginH  = checkCreds cs jwts
    staticH = serveDirectoryWith $ set
      where
        set = 
          (defaultWebAppSettings $ error "unused") 
            { ssLookupFile = ssLookupFile embedded
            , ssIndices = map unsafeToPiece ["index.html"] }
        embedded = embeddedSettings staticFiles
```

`staticFiles` declares the lookup name we will use in `Raw` and the Template 
Haskell code `$(embedFile ...)` will load the file contents at compile time. 
`index.html` is where we store the compiled Elm code. The last piece of interest
is `ssIndices`. This tells haskell to treat a root lookup `/` as `/index.html`.

Take a look at the cabal file and `Setup.hs`. I have added `elm/src/Main.elm` to 
`data-dirs` and some custom code so that when you run `stack build` it will 
automatically build the elm code and then build the servant project if there 
have been changes to either the Elm or the Haskell project.

## Elm

The front-end for this example can do four things: check if the user is logged 
in upon loading the page, log in, log out and request a die roll from the server
if the user is authenticated.

#### Native Code 

As mentioned [here](https://github.com/plow-technologies/servant-auth#csrf-and-the-frontend). 
Each query to an authenticated requires the front-end to set `X-XSRF-TOKEN` in 
the header. In order to do this, we need to write native JavaScript code that 
is called by Elm. To do so, we must name the function for the user and project 
name in the repository key in elm-package.json. When you initiate a new elm 
project it will intially set as `user` and `project`. 

```JavaScript
var _mchaver$servant_auth_and_elm_example$Native_CsrfCookie = function() {

  var scheduler = _elm_lang$core$Native_Scheduler;
  var value = {};

  value.csrfCookie = e => {
    return scheduler.nativeBinding(callback => {
      let r = document.cookie.match(new RegExp('XSRF-TOKEN=([^;]+)'));
      if (r) {
        callback(scheduler.succeed(r[1]));
      } else {
        callback(scheduler.fail([]));
      }
    })
  };
  
  value.deleteCookie = e => {
    return scheduler.nativeBinding(callback => {
      document.cookie = 'XSRF-TOKEN=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;';
      window.location.href = '/';
      callback(scheduler.succeed([]));
    })
  };

  return value;

}();
```

The `csrfCookie` function gets `XSRF-TOKEN` from the cookies and sends it to 
Elm. `deleteCookie` sets the `XSRF-TOKEN` to an expired date to delete it and 
logout the user. Then it redirects the user to the root path. There is a small 
layer of code in [CsrfCookie.elm](https://github.com/mchaver/servant-auth-and-elm-example/blob/master/elm/src/CsrfCookie.elm)
to call these two functions from Elm.

Then there is a function `sendWithCsrfToken` using [elm-http-builder](https://github.com/lukewestby/elm-http-builder)
to call `csrfCookie` and set `X-XSRF-TOKEN` for any http-builder request that 
you give it. Finally, here is an example of making a simple authenticated 
request.

```Elm
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RollDieRequest ->
      let
        request = 
          HttpB.get "/die/roll" 
          |> HttpB.withExpect (Http.expectJson Json.Decode.int)
      in
        (model, sendWithCsrfToken RollDieResponse request)
```

## Resources

- [Github :: servant-auth-and-elm-example](https://github.com/mchaver/servant-auth-and-elm-example)

- [Github :: elm-http-builder](https://github.com/lukewestby/elm-http-builder)

- [Github :: servant-auth](https://github.com/plow-technologies/servant-auth)

- [Hackage :: jose: Javascript Object Signing and Encryption and JSON Web Token library](http://hackage.haskell.org/package/jose)

- [Hackage :: servant-auth](http://hackage.haskell.org/package/servant-auth)

- [Hackage :: servant-auth-server](http://hackage.haskell.org/package/servant-auth-server)

- [JSON Web Token Introduction](https://jwt.io/introduction/)

- [Wikipedia :: JSON Web Token](https://en.wikipedia.org/wiki/JSON_Web_Token)
