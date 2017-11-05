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
import Servant ((:~>), Handler, runReaderTNat)
import Servant.Server.Experimental.Auth.Cookie
       (AuthCookieSettings, PersistentServerKey, RandomSource)

data AppContext = AppContext
  { appContextPool :: Pool Connection
  , appContextAuthSettings :: AuthCookieSettings
  , appContextRandomSource :: RandomSource
  , appContextServerKey :: PersistentServerKey
  , appContextApproot :: String
  , appContextPort :: Int
  , appContextScheme :: String
  }

type App = ReaderT AppContext Handler

convert :: AppContext -> (App :~> Handler)
convert = runReaderTNat

runDB :: forall a. Fetch a -> App (Maybe a)
runDB op = do
  pool <- asks appContextPool
  withResource pool $ \con -> liftIO $ runMaybeT (runReaderT op con)
