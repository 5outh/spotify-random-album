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
  max-width: 400px;
}

h3 {
  line-height: 0.25em;
  color: #191414;
  text-align: center;
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
  color: #FFFFFF;
  background-color: #191414;
  padding: 14px 28px;
  font-size: 20px;
  cursor: pointer;
  text-align: center;
}
|]