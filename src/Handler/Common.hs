{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Handler.Common where

import           Data.FileEmbed (embedFile)
import           Import

getFaviconR ∷ Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")
