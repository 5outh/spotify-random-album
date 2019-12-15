{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Html.RandomizeForm where

import           Prelude                     (($))
import qualified Prelude                     as P

import           Data.Foldable               (for_)
import           Data.String
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A
import           Text.RawString.QQ
import           Types.Spotify

randomizeForm SpotifyDevices {..} = (form ! A.action "/randomize") $ do
  select ! A.id "device_id" ! A.name "device_id" $ do
    for_ spotifyDevicesDevices $ \SpotifyDevice {..} ->
      let setSelected = if spotifyDeviceIsActive
            then (! A.selected (fromString spotifyDeviceId))
            else P.id
      in  (setSelected $ option ! A.value (fromString spotifyDeviceId))
            (toHtml spotifyDeviceName)
  button "Randomize Album"
