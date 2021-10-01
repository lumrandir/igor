{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Settings where

import           ClassyPrelude.Yesod
import qualified Control.Exception           as Exception
import           Data.Aeson                  (Result (..), fromJSON, withObject,
                                              (.!=), (.:?))
import           Data.FileEmbed
import           Data.Yaml                   (decodeEither')
import           Database.Persist.Postgresql
import           Language.Haskell.TH.Syntax  (Exp, Q)
import           Network.Wai.Handler.Warp    (HostPreference)
import           Yesod.Default.Config2
import           Yesod.Default.Util          (WidgetFileSettings,
                                              widgetFileNoReload,
                                              widgetFileReload)

data AppSettings
  = AppSettings
      { appDatabaseConf           :: PostgresConf
      , appHost                   :: HostPreference
      , appPort                   :: Int
      , appStaticDir              :: String
      , appMutableStatic          :: Bool
      , appDetailedRequestLogging :: Bool
      , appIpFromHeader           :: Bool
      , appRoot                   :: Maybe Text
      , appSkipCombining          :: Bool
      , appReloadTemplates        :: Bool
      , appShouldLogAll           :: Bool
      }

configSettingsYmlValue ∷ Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

configSettingsYmlBS ∷ ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    let defaultDev =
#ifdef DEVELOPMENT
          True
#else
          False
#endif
    appStaticDir <- o.: "static-dir"
    appDatabaseConf <- o.: "database"
    appRoot <- o .:? "approot"
    appHost <- fromString <$> o .: "host"
    appPort <- o .: "port"
    appIpFromHeader <- o .: "ip-from-header"
    dev <- o .:? "development" .!= defaultDev
    appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
    appShouldLogAll <- o .:? "should-log-all" .!= dev
    appReloadTemplates <- o .:? "reload-templates" .!= dev
    appMutableStatic <- o .:? "mutable-static" .!= dev
    appSkipCombining <- o .:? "skip-combining" .!= dev

    return AppSettings { .. }

widgetFile ∷ String → Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                 then widgetFileReload
                 else widgetFileNoReload)
              widgetFileSettings

compileTimeAppSettings ∷ AppSettings
compileTimeAppSettings =
  case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
    Error e          -> error e
    Success settings -> settings

widgetFileSettings ∷ WidgetFileSettings
widgetFileSettings = def
