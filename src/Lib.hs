{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
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
import           Text.Blaze.Html5              (toHtml, (!))
import qualified Text.Blaze.Html5              as Blaze
import qualified Text.Blaze.Html5.Attributes   as Blaze hiding (form, label)
import           Web.Cookie                    (defaultSetCookie)
import           Web.Scotty
import qualified Web.Scotty                    as Scotty

stateKey = "spotify_auth_state"
accessTokenKey = "access_token"
refreshTokenKey = "refresh_token"
sessionKey = "session"
redirectUri = "http://localhost:8888/callback"

appMain :: IO ()
appMain = do
  clientId     <- getEnv "SPOTIFY_CLIENT_ID"
  clientSecret <- getEnv "SPOTIFY_CLIENT_SECRET"
  session      <- Vault.newKey
  store        <- mapStore_ @String

  let token = Base64.encode (S8.pack clientId <> ":" <> S8.pack clientSecret)
      authorization = "Basic " <> token
      sessionMiddleware = withSession store sessionKey defaultSetCookie session

  scotty 8888 $ do
    Scotty.middleware sessionMiddleware

    Scotty.get "/" $ do
      Scotty.html $ Blaze.renderHtml $ do
        Blaze.form ! Blaze.action "/login" $ do
          Blaze.button "Login with Spotify"

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
                , "&redirect_uri=http://localhost:8888/callback"
                , "&state=" <> state
                ]
      redirect url

    Scotty.get "/callback" $ do
      code  <- param "code"

      -- Store state in a cookie
      state <- param "state"
      mStoredState :: Maybe S8.ByteString <- sessionLookup session stateKey
      when (Just state /= mStoredState) $ error "mismatched state"
      -- TODO session clear
      -- sessionInsert stateKey Nothing

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

      randomizeForm <- getRandomizeForm accessToken
      Scotty.html $ Blaze.renderHtml randomizeForm

    Scotty.post "/randomize" $ do
      deviceId           <- param @S8.ByteString "device_id"
      Just accessTokenBS <- sessionLookup session accessTokenKey
      let accessToken = T.pack $ S8.unpack accessTokenBS

      albums      <- liftIO $ getMyAlbums accessToken
      randomAlbum <- liftIO $ uniform albums

      let tracks =
            spotifyTracksItems
              . spotifyAlbumTracks
              . spotifyAlbumItemAlbum
              $ randomAlbum

          uris = map spotifyTrackUri tracks
          ids  = map spotifyTrackId tracks

      albumAudioFeatures :: SpotifyAudioFeatures <-
        liftIO
        $ callSpotifyWith accessToken
                          "GET https://api.spotify.com/v1/audio-features"
        $ setRequestQueryString
            [("ids", Just $ S8.pack $ List.intercalate "," ids)]

      liftIO
        $ callSpotifyWithNoResponse
            accessToken
            "PUT https://api.spotify.com/v1/me/player/play"
        $ setRequestBodyJSON (object ["uris" Aeson..= uris])
        . setRequestQueryString [("device_id", Just deviceId)]

      randomizeForm <- getRandomizeForm accessToken
      Scotty.html $ Blaze.renderHtml $ do
        Blaze.h1
          (fromString $ "Playing album: " <> spotifyAlbumName
            (spotifyAlbumItemAlbum randomAlbum)
          )
        randomizeForm

        Blaze.h2 "Stats"
        Blaze.p $ fromString $ "danceability: " <> formatAsPercentage
          (averageFeature spotifyAudioFeatureDanceability albumAudioFeatures)
        Blaze.p $ fromString $ "energy: " <> formatAsPercentage
          (averageFeature spotifyAudioFeatureEnergy albumAudioFeatures)
        Blaze.p $ fromString $ "speechiness: " <> formatAsPercentage
          (averageFeature spotifyAudioFeatureSpeechiness albumAudioFeatures)
        Blaze.p $ fromString $ "acousticness: " <> formatAsPercentage
          (averageFeature spotifyAudioFeatureAcousticness albumAudioFeatures)
        Blaze.p $ fromString $ "instrumentalness: " <> formatAsPercentage
          (averageFeature spotifyAudioFeatureInstrumentalness albumAudioFeatures
          )
        Blaze.p $ fromString $ "liveness: " <> formatAsPercentage
          (averageFeature spotifyAudioFeatureLiveness albumAudioFeatures)

formatAsPercentage d = show (floor (d * 100)) <> "%"

getMyAlbums accessToken = go [] "https://api.spotify.com/v1/me/albums?limit=50"
 where
  go albums url = do
    SpotifyAlbumsResponse {..} <- callSpotifyWith accessToken ("GET " <> url) id
    case spotifyAlbumsResponseNext of
      Nothing   -> pure (spotifyAlbumsResponseItems ++ albums)
      Just next -> do
        print next
        (spotifyAlbumsResponseItems ++) <$> go albums next

data SpotifyDevices = SpotifyDevices
  { spotifyDevicesDevices       :: [SpotifyDevice]
  } deriving (Show, Generic)

instance FromJSON SpotifyDevices where
  parseJSON = genericParseJSON (unPrefix "spotifyDevices")
instance ToJSON SpotifyDevices where
  toJSON = genericToJSON (unPrefix "spotifyDevices")
  toEncoding = genericToEncoding (unPrefix "spotifyDevices")

data SpotifyDevice = SpotifyDevice
  { spotifyDeviceId               :: String
  , spotifyDeviceIsActive         :: Bool
  , spotifyDeviceName             :: String
  , spotifyDeviceIsPrivateSession :: Bool
  , spotifyDeviceIsRestricted     :: Bool
  , spotifyDeviceVolumePercent    :: Double
  } deriving (Show, Generic)

instance FromJSON SpotifyDevice where
  parseJSON = genericParseJSON (unPrefix "spotifyDevice")

instance ToJSON SpotifyDevice where
  toJSON = genericToJSON (unPrefix "spotifyDevice")
  toEncoding = genericToEncoding (unPrefix "spotifyDevice")

unPrefix str = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop (length (str :: String))
  }

data SpotifyAlbumsResponse = SpotifyAlbumsResponse
  { spotifyAlbumsResponseItems :: [SpotifyAlbumItem]
  , spotifyAlbumsResponseNext  :: Maybe String
  } deriving (Show, Generic)

instance ToJSON SpotifyAlbumsResponse where
  toJSON = genericToJSON $ unPrefix "spotifyAlbumsResponse"
instance FromJSON SpotifyAlbumsResponse where
  parseJSON = genericParseJSON $ unPrefix "spotifyAlbumsResponse"

data SpotifyAlbumItem = SpotifyAlbumItem
  { spotifyAlbumItemAlbum :: SpotifyAlbum
  } deriving (Show, Generic)

instance ToJSON SpotifyAlbumItem where
  toJSON = genericToJSON $ unPrefix "spotifyAlbumItem"
instance FromJSON SpotifyAlbumItem where
  parseJSON = genericParseJSON $ unPrefix "spotifyAlbumItem"

data SpotifyAlbum = SpotifyAlbum
  { spotifyAlbumUri    :: String
  , spotifyAlbumName   :: String
  , spotifyAlbumTracks :: SpotifyTracks
  } deriving (Show, Generic)

instance ToJSON SpotifyAlbum where
  toJSON = genericToJSON $ unPrefix "spotifyAlbum"
instance FromJSON SpotifyAlbum where
  parseJSON = genericParseJSON $ unPrefix "spotifyAlbum"

data SpotifyTracks = SpotifyTracks { spotifyTracksItems :: [SpotifyTrack], spotifyTracksNext  :: Maybe String }
  deriving (Show, Generic)

instance ToJSON SpotifyTracks where
  toJSON = genericToJSON $ unPrefix "spotifyTracks"
instance FromJSON SpotifyTracks where
  parseJSON = genericParseJSON $ unPrefix "spotifyTracks"

data SpotifyTrack = SpotifyTrack { spotifyTrackUri :: String, spotifyTrackId :: String }
  deriving (Show, Generic)

instance ToJSON SpotifyTrack where
  toJSON = genericToJSON $ unPrefix "spotifyTrack"
instance FromJSON SpotifyTrack where
  parseJSON = genericParseJSON $ unPrefix "spotifyTrack"

data SpotifyAudioFeatures = SpotifyAudioFeatures { spotifyAudioFeaturesAudioFeatures :: [SpotifyAudioFeature] }
  deriving (Show, Generic)

averageFeature
  :: (SpotifyAudioFeature -> Double) -> SpotifyAudioFeatures -> Double
averageFeature feature (SpotifyAudioFeatures features) = average
  $ map feature features
  where average xs = sum xs / List.genericLength xs

instance ToJSON SpotifyAudioFeatures where
  toJSON = genericToJSON $ unPrefix "spotifyAudioFeatures"
instance FromJSON SpotifyAudioFeatures where
  parseJSON = genericParseJSON $ unPrefix "spotifyAudioFeatures"

data SpotifyAudioFeature = SpotifyAudioFeature
  { spotifyAudioFeatureDanceability     :: Double
  , spotifyAudioFeatureEnergy           :: Double
  , spotifyAudioFeatureKey              :: Double
  , spotifyAudioFeatureLoudness         :: Double
  , spotifyAudioFeatureMode             :: Double
  , spotifyAudioFeatureSpeechiness      :: Double
  , spotifyAudioFeatureAcousticness     :: Double
  , spotifyAudioFeatureInstrumentalness :: Double
  , spotifyAudioFeatureLiveness         :: Double
  , spotifyAudioFeatureValence          :: Double
  , spotifyAudioFeatureTempo            :: Double
  , spotifyAudioFeatureId               :: String
  , spotifyAudioFeatureUri              :: String
  }
  deriving (Show, Generic)

instance ToJSON SpotifyAudioFeature where
  toJSON = genericToJSON $ unPrefix "spotifyAudioFeature"
instance FromJSON SpotifyAudioFeature where
  parseJSON = genericParseJSON $ unPrefix "spotifyAudioFeature"

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
        Error   str -> error str
        Success a   -> pure a
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
  let Just (_sessionLookup, sessionInsert0) =
        Vault.lookup session0 (vault request0)
  sessionInsert0 key val

sessionLookup session0 key = do
  request0 <- request
  let Just (sessionLookup0, _sessionInsert) =
        Vault.lookup session0 (vault request0)
  sessionLookup0 key

getRandomizeForm accessToken = do
  SpotifyDevices {..} <- liftIO $ callSpotifyWith
    accessToken
    "GET https://api.spotify.com/v1/me/player/devices"
    id

  pure
    $ (Blaze.form ! Blaze.action "/randomize" ! Blaze.method "POST")
    $ do
        (Blaze.label ! Blaze.for "device_id") "Choose a Device"
        Blaze.select ! Blaze.id "device_id" ! Blaze.name "device_id" $ do
          for_ spotifyDevicesDevices $ \SpotifyDevice {..} ->
            let setSelected = if spotifyDeviceIsActive
                  then (! Blaze.selected (fromString spotifyDeviceId))
                  else id
            in
              (setSelected $ Blaze.option ! Blaze.value
                  (fromString spotifyDeviceId)
                )
                (toHtml spotifyDeviceName)
        -- (Blaze.input ! Blaze.type_ "radio") "Danceable"
        -- (Blaze.label ! Blaze.for "danceability") "Danceable"
        -- (Blaze.label ! Blaze.for "none") "none"

        Blaze.button "Randomize Album"
