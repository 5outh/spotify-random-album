
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Html.Layout
  ( withLayout
  )
where

import           Prelude                     (($), (<>))

import           Data.String
import           Data.Text                   (Text)
import           Html.Header
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A
import           Text.RawString.QQ

withLayout :: Html -> Html
withLayout content = do
  Html.Header.header
  div ! A.class_ "container" $ do
    div ! A.class_ "content" $ do
      content
      footer $ do
        (p ! A.style "font-size: 10px") "Made with ❤️ by Ben Kovach"
