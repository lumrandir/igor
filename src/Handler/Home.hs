{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Handler.Home where

import           Import


getRootR ∷ Handler Html
getRootR =
  defaultLayout $ do
    setTitle "Igor Tonet"
