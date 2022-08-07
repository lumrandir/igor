{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Handler.Home where

import           Import


getRootR ∷ Handler Html
getRootR =
  defaultLayout $ do
    setTitle "Igor Tonet"
    addScript $ StaticR js_app_js
    addStylesheet $ StaticR css_app_css
