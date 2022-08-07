{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}
module Settings.StaticFiles where

import           Yesod.Static (staticFilesList)

staticFilesList "static" ["js/app.js", "css/app.css"]

