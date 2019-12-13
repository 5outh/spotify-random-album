#! /usr/bin/env stack
-- stack script --resolver lts-14.16 --package lens --package scotty --package http-client --package http-types --package random --package http-conduit --package text --package aeson --package base64-bytestring --package bytestring --package lens-aeson

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           GHC.Generics
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           System.Environment
import           System.Random
import           Web.Scotty
import qualified Web.Scotty                 as Scotty

stateKey = "spotify_auth_state"
redirectUri = "http://localhost:8888/callback"

main :: IO ()
main = do
    clientId <- getEnv "SPOTIFY_CLIENT_ID"
    clientSecret <- getEnv "SPOTIFY_CLIENT_SECRET"

    let
      token = Base64.encode (S8.pack clientId <> ":" <> S8.pack clientSecret)
      authorization = "Basic " <> token

    scotty 8888 $ do
      Scotty.get "/login" $ do
        state <- liftIO $ fmap S8.pack $ replicateM 16 $ randomRIO ('a', 'z')
        -- setCookie stateKey state

        let url = TL.pack $ S8.unpack $ ("https://accounts.spotify.com/authorize?" <>) $
               mconcat
              [ "response_type=code"
              , "&client_id=" <> S8.pack clientId
              , "&scope=user-read-private user-read-email user-read-playback-state user-modify-playback-state"
              , "&redirect_uri=http://localhost:8888/callback"
              , "&state=" <> state
              ]
        liftIO $ print url
        redirect url

      Scotty.get "/callback" $ do
        code <- param "code"

        -- _mState <- param "state"
        -- NB. this is important in prod but who cares here
        -- mStoredState <- (readCookie stateKey) =<< getCookies
        -- when mState != mStoredState $
          -- error "Mismatched state"
        -- clearCookie stateKey
        --

        request0 <- parseRequest "POST https://accounts.spotify.com/api/token"

        let
          request
            = setRequestBodyURLEncoded [("grant_type", "authorization_code"), ("code", code), ("redirect_uri", redirectUri)]
            $ setRequestHeader "Authorization" [authorization] request0

        response <- httpJSON request

        let
          responseValue = getResponseBody @Value response

        liftIO $ print responseValue
        let
          Just accessToken = responseValue ^? key "access_token" . _String
          Just refreshToken = responseValue ^? key "refresh_token" . _String

        -- res <- liftIO $ callSpotifyWith accessToken "GET https://api.spotify.com/v1/me" id
        -- res :: SpotifyDevices <- liftIO
          -- $ callSpotifyWith accessToken "GET https://api.spotify.com/v1/me/player/devices" id
        liftIO $ callSpotifyWithNoResponse accessToken "PUT https://api.spotify.com/v1/me/player/play"
          $ setRequestBodyJSON (object ["uris" Aeson..= [("spotify:track:3RRBYeYWJk8b3HH76HTSPI" :: String)]])

        Scotty.json ()

    -- val <- callSpotifyWith accessToken "GET https://api.spotify.com/v1/me/" id
    -- print val

callSpotifyWith :: forall a. (FromJSON a, Show a) => T.Text ->  String -> (Request -> Request) -> IO a
callSpotifyWith accessToken request f = do
  request0 <- parseRequest request

  let
    spotifyAuthorization = "Bearer " <> S8.pack (T.unpack accessToken)
    request
      = setRequestHeader "Authorization" [spotifyAuthorization]
      $ setRequestHeader "Content-Type" ["application/json"]
      $ f request0

  response <- httpJSON request
  print response
  pure $ getResponseBody @a response

callSpotifyWithNoResponse :: T.Text ->  String -> (Request -> Request) -> IO ()
callSpotifyWithNoResponse accessToken request f = do
  request0 <- parseRequest request

  let
    spotifyAuthorization = "Bearer " <> S8.pack (T.unpack accessToken)
    request
      = setRequestHeader "Authorization" [spotifyAuthorization]
      $ setRequestHeader "Content-Type" ["application/json"]
      $ f request0

  response <- httpNoBody request
  if getResponseStatusCode response == 204 then pure () else error (show response)

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

-- {"devices":[{"volume_percent":100,"name":"Amanda’s MacBook Pro","id":"f3d2ecab0608ddacf23d44489813cdc4b503848d","is_restricted":false,"type":"Computer","is_active":true,"is_private_session":false}]}

unPrefix str = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop (length (str :: String)) }
