{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Lib
  ( appMain
  )
where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Data.Aeson
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Base64        as Base64
import qualified Data.ByteString.Char8         as S8
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Foldable
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Data.String
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Traversable
import           Data.Vault.Lazy               as Vault
import           GHC.Generics
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Network.Wai                   (vault)
import           Network.Wai.Session
import           Network.Wai.Session.Map
import           System.Environment
import           System.Random
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import           Text.Blaze.Html5              (style, toHtml, (!))
import qualified Text.Blaze.Html5              as Blaze hiding (style)
import qualified Text.Blaze.Html5.Attributes   as Blaze hiding (form, label,
                                                         span)
import           Types.Spotify
import           Web.Cookie                    (defaultSetCookie)
import           Web.Scotty
import qualified Web.Scotty                    as Scotty

import           Html.Header
import qualified Html.Header                   as Header
import           Html.Layout
import           Html.RandomizeForm

-- Session keys
stateKey = "spotify_auth_state"
accessTokenKey = "access_token"
refreshTokenKey = "refresh_token"
sessionKey = "session"

appMain :: IO ()
appMain = do
  clientId     <- getEnv "SPOTIFY_CLIENT_ID"
  clientSecret <- getEnv "SPOTIFY_CLIENT_SECRET"
  port         <- read <$> getEnv "PORT"
  redirectUri  <- S8.pack <$> getEnv "REDIRECT_URI"
  session      <- Vault.newKey
  store        <- mapStore_ @String

  let token = Base64.encode (S8.pack clientId <> ":" <> S8.pack clientSecret)
      authorization = "Basic " <> token
      sessionMiddleware = withSession store sessionKey defaultSetCookie session

  scotty port $ do
    Scotty.middleware sessionMiddleware

    Scotty.get "/" $ do
      renderPage $ do
        Blaze.h1 "Spotify Randomizer"
        Blaze.h3 "Play a random album from your Spotify collection"
        Blaze.a ! Blaze.class_ "spotify-login-button" ! Blaze.href "/login" $ do
          Blaze.i ! Blaze.class_ "fab fa-spotify" $ Blaze.text ""
          Blaze.text " Login with Spotify"

    Scotty.get "/login" $ do
      state <- liftIO $ fmap S8.pack $ replicateM 16 $ randomRIO ('a', 'z')

      -- Store state in a cookie
      sessionInsert session stateKey state

      let
        url =
          TL.pack
            $ S8.unpack
            $ ("https://accounts.spotify.com/authorize?" <>)
            $ mconcat
                [ "response_type=code"
                , "&client_id=" <> S8.pack clientId
                , "&scope=user-read-private user-read-email user-read-playback-state user-modify-playback-state user-library-read"
                , "&redirect_uri=" <> redirectUri
                , "&state=" <> state
                ]
      redirect url

    Scotty.get "/callback" $ do
      code  <- param "code"

      -- Store state in a cookie
      state <- param "state"
      mStoredState :: Maybe S8.ByteString <- sessionLookup session stateKey
      when (Just state /= mStoredState) $ error "mismatched state"

      request0 <- parseRequest "POST https://accounts.spotify.com/api/token"

      let request =
            setRequestBodyURLEncoded
                [ ("grant_type"  , "authorization_code")
                , ("code"        , code)
                , ("redirect_uri", redirectUri)
                ]
              $ setRequestHeader "Authorization" [authorization] request0

      tokenResponse <- httpJSON request

      let
        tokenResponseValue = getResponseBody @Value tokenResponse
        Just accessToken   = tokenResponseValue ^? key "access_token" . _String
        Just refreshToken  = tokenResponseValue ^? key "refresh_token" . _String

      sessionInsert session accessTokenKey  (S8.pack $ T.unpack accessToken)
      sessionInsert session refreshTokenKey (S8.pack $ T.unpack refreshToken)
      redirect "/choose"

    Scotty.get "/choose" $ do

      mAccessTokenBS <- sessionLookup session accessTokenKey
      let mAccessToken = fmap (T.pack . S8.unpack) mAccessTokenBS
      case mAccessToken of
        Nothing          -> redirect "/login"
        Just accessToken -> do
          devices@SpotifyDevices {..} <- getDevices accessToken
          case spotifyDevicesDevices of
            [] -> renderPage $ do
              Blaze.p $ mconcat
                [ "No Spotify Devices Found."
                , " Make sure that Spotify is active on at least one device and refresh this page"
                , " (sometimes, you'll need to start playing a song to mark the device as active)."
                ]
            _ -> renderPage $ randomizeForm devices

    Scotty.get "/randomize" $ do
      deviceId       <- param @S8.ByteString "device_id"
      mAccessTokenBS <- sessionLookup session accessTokenKey
      let mAccessToken = fmap (T.pack . S8.unpack) mAccessTokenBS
      case mAccessToken of
        Nothing          -> redirect "/login"
        Just accessToken -> do
          albums      <- liftIO $ getMyAlbums accessToken
          randomAlbum <- liftIO $ uniform albums

          let
            tracks =
              spotifyTracksItems
                . spotifyAlbumTracks
                . spotifyAlbumItemAlbum
                $ randomAlbum
            album       = spotifyAlbumItemAlbum randomAlbum
            mSpotifyUrl = Map.lookup "spotify" (spotifyAlbumExternalUrls album)
            uris        = map spotifyTrackUri tracks
            ids         = map spotifyTrackId tracks

          liftIO
            $ callSpotifyWithNoResponse
                accessToken
                "PUT https://api.spotify.com/v1/me/player/play"
            $ setRequestBodyJSON (object ["uris" Aeson..= uris])
            . setRequestQueryString [("device_id", Just deviceId)]

          devices <- getDevices accessToken

          let imageUrl = case spotifyAlbumImages album of
                []                      -> "#"
                (SpotifyImage {..} : _) -> spotifyImageUrl
              albumTitle = case mSpotifyUrl of
                Nothing -> Blaze.a
                Just spotifyUrl ->
                  (Blaze.a ! Blaze.href (fromString spotifyUrl) ! Blaze.target
                    "_blank"
                  )

          renderPage $ do
            Blaze.div ! Blaze.style "margin-left:auto; margin-right:auto" $ do
              Blaze.h2 $ do
                albumTitle (fromString $ spotifyAlbumName album)
                fromString
                  $  " ("
                  <> (take 4 $ spotifyAlbumReleaseDate album)
                  <> ")"

              Blaze.h3
                ( fromString
                $ List.intercalate ", "
                $ map spotifyArtistSimpleName
                $ spotifyAlbumArtists album
                )

              Blaze.h2
                (fromString $ List.intercalate ", " $ spotifyAlbumGenres album)

              Blaze.img ! Blaze.src (fromString imageUrl) ! Blaze.style
                "max-height:440px"

              randomizeForm devices

renderFeature albumAudioFeatures name feature =
  Blaze.p $ fromString $ (name <> ": ") <> formatAsPercentage
    (averageFeature feature albumAudioFeatures)

formatAsPercentage d = show (floor (d * 100)) <> "%"

getMyAlbums accessToken = go [] "https://api.spotify.com/v1/me/albums?limit=50"
 where
  go albums url = do
    SpotifyAlbumsResponse {..} <- callSpotifyWith accessToken ("GET " <> url) id
    case spotifyAlbumsResponseNext of
      Nothing   -> pure (spotifyAlbumsResponseItems ++ albums)
      Just next -> (spotifyAlbumsResponseItems ++) <$> go albums next

averageFeature
  :: (SpotifyAudioFeature -> Double) -> SpotifyAudioFeatures -> Double
averageFeature feature (SpotifyAudioFeatures features) = average
  $ map feature features
  where average xs = sum xs / List.genericLength xs

callSpotifyWith
  :: forall a
   . (FromJSON a, Show a)
  => T.Text
  -> String
  -> (Request -> Request)
  -> IO a
callSpotifyWith accessToken request f = do
  request0 <- parseRequest request

  let spotifyAuthorization = "Bearer " <> S8.pack (T.unpack accessToken)
      request =
        setRequestHeader "Authorization" [spotifyAuthorization]
          $ setRequestHeader "Content-Type" ["application/json"]
          $ f request0

  response <- httpJSON request
  let responseBodyValue = getResponseBody @Value response
  if getResponseStatusCode response < 300
    then do
      case fromJSON @a responseBodyValue of
        Error str -> do
          print responseBodyValue
          error str
        Success a -> pure a
    else do
      print responseBodyValue
      error (show response)

callSpotifyWithNoResponse :: T.Text -> String -> (Request -> Request) -> IO ()
callSpotifyWithNoResponse accessToken request f = do
  request0 <- parseRequest request

  let spotifyAuthorization = "Bearer " <> S8.pack (T.unpack accessToken)
      request =
        setRequestHeader "Authorization" [spotifyAuthorization]
          $ setRequestHeader "Content-Type" ["application/json"]
          $ f request0

  response <- httpNoBody request
  if getResponseStatusCode response == 204
    then pure ()
    else do
      response1 <- httpJSON request
      print (getResponseBody response1 :: Value)
      error
        ("callSpotifyWithNoResponse Failure: " <> show response1 <> "\n" <> show
          (getResponseBody response1)
        )

sessionInsert session0 key val = do
  request0 <- request
  let mSessionAccess = Vault.lookup session0 (vault request0)
  case mSessionAccess of
    Nothing                  -> error "impossible: session is not available"
    Just (_, sessionInsert0) -> sessionInsert0 key val

sessionLookup session0 key = do
  request0 <- request
  let mSessionAccess = Vault.lookup session0 (vault request0)
  case mSessionAccess of
    Nothing                  -> pure Nothing
    Just (sessionLookup0, _) -> sessionLookup0 key

getDevices accessToken = liftIO $ callSpotifyWith
  accessToken
  "GET https://api.spotify.com/v1/me/player/devices"
  id

renderPage = Scotty.html . Blaze.renderHtml . withLayout
