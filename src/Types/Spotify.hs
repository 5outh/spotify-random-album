module Types.Spotify where

import           Data.Aeson
import qualified Data.Aeson      as Aeson
import           Data.Map.Strict (Map)
import           GHC.Generics

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
  { spotifyAlbumUri          :: String
  , spotifyAlbumHref         :: String
  , spotifyAlbumExternalUrls :: Map String String
  , spotifyAlbumReleaseDate  :: String
  , spotifyAlbumGenres       :: [String]
  , spotifyAlbumName         :: String
  , spotifyAlbumTracks       :: SpotifyTracks
  , spotifyAlbumImages       :: [SpotifyImage]
  , spotifyAlbumArtists      :: [SpotifyArtistSimple]
  } deriving (Show, Generic)

instance ToJSON SpotifyAlbum where
  toJSON = genericToJSON $ unPrefix "spotifyAlbum"
instance FromJSON SpotifyAlbum where
  parseJSON = genericParseJSON $ unPrefix "spotifyAlbum"

data SpotifyImage = SpotifyImage
  { spotifyImageUrl :: String
  }
  deriving (Show, Generic)

instance ToJSON SpotifyImage where
  toJSON = genericToJSON $ unPrefix "spotifyImage"
instance FromJSON SpotifyImage where
  parseJSON = genericParseJSON $ unPrefix "spotifyImage"

data SpotifyArtistSimple = SpotifyArtistSimple
  { spotifyArtistSimpleName :: String
  }
  deriving (Show, Generic)

instance ToJSON SpotifyArtistSimple where
  toJSON = genericToJSON $ unPrefix "spotifyArtistSimple"
instance FromJSON SpotifyArtistSimple where
  parseJSON = genericParseJSON $ unPrefix "spotifyArtistSimple"

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


unPrefix str = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop (length (str :: String))
  }
