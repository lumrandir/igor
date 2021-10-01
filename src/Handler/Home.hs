{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Handler.Home where

import           Import

getHomeR ∷ Handler Html
getHomeR = do
  defaultLayout $ do
    setTitle "Igor Tonet"
