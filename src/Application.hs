{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
  ( appMain
  , develMain
  ) where

import           Control.Monad.Logger                 (liftLoc, runLoggingT)
import           Database.Persist.Postgresql          (createPostgresqlPool,
                                                       pgConnStr, pgPoolSize,
                                                       runSqlPool)
import           Handler.Common
import           Handler.Home
import           Import
import           Language.Haskell.TH.Syntax           (qLocation)
import           Network.HTTP.Client.TLS              (getGlobalManager)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       defaultShouldDisplayException,
                                                       runSettings, setHost,
                                                       setOnException, setPort)
import           Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                                       IPAddrSource (..),
                                                       OutputFormat (..),
                                                       destination,
                                                       mkRequestLogger,
                                                       outputFormat)
import           System.Log.FastLogger                (defaultBufSize,
                                                       newStdoutLoggerSet,
                                                       toLogStr)

mkYesodDispatch "App" resourcesApp

appMain ∷ IO ()
appMain = do
  settings <- loadYamlSettingsArgs
      [configSettingsYmlValue]
      useEnv

  foundation <- makeFoundation settings

  app <- makeApplication foundation

  runSettings (warpSettings foundation) app

develMain ∷ IO ()
develMain = develMainHelper getApplicationDev

getApplicationDev ∷ IO (Settings, Application)
getApplicationDev = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  return (wsettings, app)

getAppSettings ∷ IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

makeApplication ∷ App → IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  appPlain <- toWaiAppPlain foundation
  return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare ∷ App → IO Middleware
makeLogWare foundation =
  mkRequestLogger def
    { outputFormat =
        if appDetailedRequestLogging $ appSettings foundation
          then Detailed True
          else Apache
            (if appIpFromHeader $ appSettings foundation
                then FromFallback
                else FromSocket)
    , destination = Logger $ loggerSet $ appLogger foundation
    }

makeFoundation ∷ AppSettings → IO App
makeFoundation appSettings = do
  appHttpManager <- getGlobalManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
    (if appMutableStatic appSettings then staticDevel else static)
    (appStaticDir appSettings)

  let mkFoundation appConnPool = App {..}
      tempFoundation = mkFoundation $ error "connPool forced on temp foundation"
      logFunc = messageLoggerSource tempFoundation appLogger

  pool <- flip runLoggingT logFunc $ createPostgresqlPool
      (pgConnStr $ appDatabaseConf appSettings)
      (pgPoolSize $ appDatabaseConf appSettings)

  runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

  return $ mkFoundation pool

warpSettings ∷ App → Settings
warpSettings foundation =
    setPort (appPort $ appSettings foundation)
  $ setHost (appHost $ appSettings foundation)
  $ setOnException (\_req e ->
      when (defaultShouldDisplayException e) $ messageLoggerSource
        foundation
        (appLogger foundation)
        $(qLocation >>= liftLoc)
        "yesod"
        LevelError
        (toLogStr $ "Exception from Warp: " ++ show e))
    defaultSettings

