#! /usr/bin/env stack
-- stack script --resolver lts-14.16 --package lens --package scotty --package http-client --package http-types --package random --package http-conduit --package text --package aeson --package base64-bytestring --package bytestring --package lens-aeson --package MonadRandom

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Random
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
      Scotty.get "/" $ do
        Scotty.html randomizeForm

      Scotty.get "/randomize" $ do
        state <- liftIO $ fmap S8.pack $ replicateM 16 $ randomRIO ('a', 'z')
        -- setCookie stateKey state

        let url = TL.pack $ S8.unpack $ ("https://accounts.spotify.com/authorize?" <>) $
               mconcat
              [ "response_type=code"
              , "&client_id=" <> S8.pack clientId
              , "&scope=user-read-private user-read-email user-read-playback-state user-modify-playback-state user-library-read"
              , "&redirect_uri=http://localhost:8888/callback"
              , "&state=" <> state
              ]
        redirect url

      Scotty.get "/callback" $ do
        code <- param "code"

        -- _mState <- param "state"
        -- NB. this is important in prod but who cares here
        -- mStoredState <- (readCookie stateKey) =<< getCookies
        -- when mState != mStoredState $
          -- error "Mismatched state"
        -- clearCookie stateKey

        request0 <- parseRequest "POST https://accounts.spotify.com/api/token"

        let
          request
            = setRequestBodyURLEncoded
              [ ("grant_type", "authorization_code")
              , ("code", code)
              , ("redirect_uri", redirectUri)
              ]
            $ setRequestHeader "Authorization" [authorization] request0

        tokenResponse <- httpJSON request

        -- TODO: Store tokens somewhere?
        let
          tokenResponseValue = getResponseBody @Value tokenResponse
          Just accessToken = tokenResponseValue ^? key "access_token" . _String
          Just refreshToken = tokenResponseValue ^? key "refresh_token" . _String

        -- res <- liftIO $ callSpotifyWith accessToken "GET https://api.spotify.com/v1/me" id
        -- TODO: Choose a Device?
        -- res :: SpotifyDevices <- liftIO
          -- $ callSpotifyWith accessToken "GET https://api.spotify.com/v1/me/player/devices" id
        albums <- liftIO $ getMyAlbums accessToken
        randomAlbum <- liftIO $ uniform albums

        liftIO $ print $ length albums

        let
          tracks = map spotifyTrackUri $ spotifyTracksItems . spotifyAlbumTracks . spotifyAlbumItemAlbum $ randomAlbum
        liftIO $ callSpotifyWithNoResponse accessToken "PUT https://api.spotify.com/v1/me/player/play"
          $ setRequestBodyJSON (object ["uris" Aeson..= tracks])

        Scotty.html $ TL.unlines
          [ "<h1>Playing album: " <> TL.pack (spotifyAlbumName $ spotifyAlbumItemAlbum randomAlbum) <> "</h1>"
          , randomizeForm
          ]

randomizeForm = "<form action=\"/randomize\"><button>Randomize Album</button><form>"

getMyAlbums accessToken = go [] "https://api.spotify.com/v1/me/albums?limit=50"
 where
  go albums url = do
    SpotifyAlbumsResponse{..} <- callSpotifyWith accessToken ("GET " <> url) id
    case spotifyAlbumsResponseNext of
      Nothing   -> pure albums
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

unPrefix str = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop (length (str :: String)) }

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

data SpotifyTrack = SpotifyTrack { spotifyTrackUri :: String }
  deriving (Show, Generic)

instance ToJSON SpotifyTrack where
  toJSON = genericToJSON $ unPrefix "spotifyTrack"
instance FromJSON SpotifyTrack where
  parseJSON = genericParseJSON $ unPrefix "spotifyTrack"

-- {
  -- "href" : "https://api.spotify.com/v1/me/albums?offset=0&limit=1",
  -- "items" : [ {
    -- "added_at" : "2015-11-26T19:13:31Z",
    -- "album" : {
      -- "album_type" : "album",
      -- "artists" : [ {} ],
      -- "id" : "5m4VYOPoIpkV0XgOiRKkWC",
      -- "name" : "In & ut",
      -- "tracks" : {
        -- "href" : "https://api.spotify.com/v1/albums/5m4VYOPoIpkV0XgOiRKkWC/tracks?offset=0&limit=50",
        -- "items" : [ {
          -- "artists" : [ {
            -- "external_urls" : {
              -- "spotify" : "https://open.spotify.com/artist/58RMTlPJKbmpmVk1AmRK3h"
            -- },
            -- "href" : "https://api.spotify.com/v1/artists/58RMTlPJKbmpmVk1AmRK3h",
            -- "id" : "58RMTlPJKbmpmVk1AmRK3h",
            -- "name" : "Abidaz",
            -- "type" : "artist",
            -- "uri" : "spotify:artist:58RMTlPJKbmpmVk1AmRK3h"
          -- }, {
            -- "external_urls" : {
              -- "spotify" : "https://open.spotify.com/artist/1l63szZeUpN1m87MOD1u7K"
            -- },
            -- "href" : "https://api.spotify.com/v1/artists/1l63szZeUpN1m87MOD1u7K",
            -- "id" : "1l63szZeUpN1m87MOD1u7K",
            -- "name" : "Chapee",
            -- "type" : "artist",
            -- "uri" : "spotify:artist:1l63szZeUpN1m87MOD1u7K"
          -- }, {
            -- "external_urls" : {
              -- "spotify" : "https://open.spotify.com/artist/1VLf7Ncxb5Jga6eyd3jh6K"
            -- },
            -- "href" : "https://api.spotify.com/v1/artists/1VLf7Ncxb5Jga6eyd3jh6K",
            -- "id" : "1VLf7Ncxb5Jga6eyd3jh6K",
            -- "name" : "C.U.P",
            -- "type" : "artist",
            -- "uri" : "spotify:artist:1VLf7Ncxb5Jga6eyd3jh6K"
          -- } ],
          -- "available_markets" : [ "AR", "AT", "AU", "BE", "BR", "CL", "CO", "CY", "CZ", "DE" ],
          -- "disc_number" : 1,
          -- "duration_ms" : 170920,
          -- "explicit" : false,
          -- "external_urls" : {
            -- "spotify" : "https://open.spotify.com/track/3VNWq8rTnQG6fM1eldSpZ0"
          -- },
          -- "href" : "https://api.spotify.com/v1/tracks/3VNWq8rTnQG6fM1eldSpZ0",
          -- "id" : "3VNWq8rTnQG6fM1eldSpZ0",
          -- "name" : "E.C.",
          -- "preview_url" : "https://p.scdn.co/mp3-preview/f95e0dba1a76b44fa2b52da2bc273d4f1c4126a5",
          -- "track_number" : 1,
          -- "type" : "track",
          -- "uri" : "spotify:track:3VNWq8rTnQG6fM1eldSpZ0"
        -- }, {
          -- ...
        -- }, {
          -- "artists" : [ {
            -- "external_urls" : {
              -- "spotify" : "https://open.spotify.com/artist/58RMTlPJKbmpmVk1AmRK3h"
            -- },
            -- "href" : "https://api.spotify.com/v1/artists/58RMTlPJKbmpmVk1AmRK3h",
            -- "id" : "58RMTlPJKbmpmVk1AmRK3h",
            -- "name" : "Abidaz",
            -- "type" : "artist",
            -- "uri" : "spotify:artist:58RMTlPJKbmpmVk1AmRK3h"
          -- } ],
          -- "available_markets" : [ "AR", "AT", "AU", "BE", "BR", "CL", "CO", "CY", "CZ", "DE", "DK", "EE" ],
          -- "disc_number" : 1,
          -- "duration_ms" : 165946,
          -- "explicit" : false,
          -- "external_urls" : {
            -- "spotify" : "https://open.spotify.com/track/6ZrVKylVlxkaXHj42O0q2r"
          -- },
          -- "href" : "https://api.spotify.com/v1/tracks/6ZrVKylVlxkaXHj42O0q2r",
          -- "id" : "6ZrVKylVlxkaXHj42O0q2r",
          -- "name" : "Råknas - Radio Edit",
          -- "preview_url" : "https://p.scdn.co/mp3-preview/a7c9a4bfa9e346e3733e9d88076ad1ae409136fb",
          -- "track_number" : 13,
          -- "type" : "track",
          -- "uri" : "spotify:track:6ZrVKylVlxkaXHj42O0q2r"
        -- } ],
        -- "limit" : 50,
        -- "next" : null,
        -- "offset" : 0,
        -- "previous" : null,
        -- "total" : 13
      -- },
      -- "type" : "album",
      -- "uri" : "spotify:album:5m4VYOPoIpkV0XgOiRKkWC"
    -- }
  -- } ],
  -- "limit" : 1,
  -- "next" : "https://api.spotify.com/v1/me/albums?offset=1&limit=1",
  -- "offset" : 0,
  -- "previous" : null,
  -- "total" : 19
-- }

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
  if getResponseStatusCode response == 204 then pure () else do
    response1 <- httpJSON request
    print (getResponseBody response1 :: Value)
    error (show response1)

