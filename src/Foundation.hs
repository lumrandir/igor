{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Foundation where

import           Database.Persist.Sql (ConnectionPool)
import           Import.NoFoundation
import           Text.Hamlet          (hamletFile)
import           Yesod.Core.Types     (Logger)

data App
  = App
      { appSettings    :: AppSettings
      , appStatic      :: Static
      , appConnPool    :: ConnectionPool
      , appHttpManager :: Manager
      , appLogger      :: Logger
      }

mkYesodData "App" $(parseRoutesFile "config/routes")

type DB a = forall (m :: * → *). MonadUnliftIO m ⇒ ReaderT SqlBackend m a

instance Yesod App where
  approot ∷ Approot App
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
       Nothing   -> getApprootText guessApproot app req
       Just root -> root

  makeSessionBackend ∷ App → IO (Maybe SessionBackend)
  makeSessionBackend _ = Just <$> defaultClientSessionBackend
    120
    "config/client_session_key.aes"

  yesodMiddleware ∷ ToTypedContent res ⇒ Handler res → Handler res
  yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware

  defaultLayout ∷ Widget → Handler Html
  defaultLayout widget = do
    pc <- widgetToPageContent $ do
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
