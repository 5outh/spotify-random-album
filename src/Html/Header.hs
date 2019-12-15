{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Html.Header where

import           Prelude                     (($))

import           Data.String
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A
import           Text.RawString.QQ

header = head $ do
  title "Spotify Randomizer"
  link
    ! A.href "https://fonts.googleapis.com/css?family=Montserrat&display=swap"
    ! A.rel "stylesheet"
  script ! A.src "https://kit.fontawesome.com/9b2f769989.js" ! customAttribute "crossorigin" "anonymous" $ do
    toHtml ("" :: String)
  style $ fromString appStyle

green = "#1DB954"
black = "#191414"

appStyle = [r|
body {
  font-family: 'Montserrat', sans-serif;
}

h3 {
  color: #191414;
  text-align: center;
  line-height: 0.5em;
}

h2 {
  text-align: center;
}

a {
  color: #1DB954;
  text-decoration: none;
}

.randomize-button {
  display: block;
  width: 100%;
  border: 3px solid #FFFFFF;
  color: #1DB954;
  padding: 14px 28px;
  font-size: 20px;
  cursor: pointer;
  text-align: center;
}

.container {
  text-align: center;
  width: 100%;
  position: relative;
  min-height: 100vh;
}

.content {
  display: inline-block;
  max-width:600px;
  padding-bottom: 2.5rem;    /* Footer height */
}

.spotify-login-button {
  background: #FFFFFF;
  margin: 14px;
  padding: 6px;
  border: 10px solid #FFFFFF;
  font-size: 24px;
}

footer {
  position: absolute;
  bottom: 0;
  width: 100%;
  height: 2.5rem;            /* Footer height */
  text-align: center;
}

.album-art {
  border: 5px solid #191414;
}
|]
