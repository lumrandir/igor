{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Foundation where

import           Database.Persist.Sql (ConnectionPool)
import           Import.NoFoundation
import           Yesod.Core.Types     (Logger)

data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static
  , appConnPool    :: ConnectionPool
  , appHttpManager :: Manager
  , appLogger      :: Logger
  }

mkYesodData "App" $(parseRoutesFile "config/routes")

type DB a = forall (m :: * -> *).
  (MonadUnliftIO m) => ReaderT SqlBackend m a

instance Yesod App where
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
       Nothing   -> getApprootText guessApproot app req
       Just root -> root
