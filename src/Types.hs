{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Types
  ( App
  , AppContext(..)
  , convert
  , runDB
  ) where

import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Pool (Pool, withResource)
import Database (Connection, Fetch)
import Servant (Handler)
import Servant.Server (hoistServer)
import Servant.Auth.Server (CookieSettings, JWTSettings)

data AppContext = AppContext
  { appContextPool :: Pool Connection
  , appContextApproot :: String
  , appContextPort :: Int
  , appContextCookieSettings :: CookieSettings
  , appContextJWTSettings :: JWTSettings
  }

type App = ReaderT AppContext Handler

--convert :: AppContext -> (App :~> Handler)
--convert = hoistServer
convert = undefined

runDB :: forall a. Fetch a -> App (Maybe a)
runDB op = do
  pool <- asks appContextPool
  withResource pool $ \con -> liftIO $ runMaybeT (runReaderT op con)
