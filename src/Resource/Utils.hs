module Resource.Utils
  ( orElseThrow
  ) where

import Control.Monad.Error.Class (MonadError)
import Servant (ServantErr, throwError)

orElseThrow :: (MonadError ServantErr m) => ServantErr -> Maybe a -> m a
orElseThrow err = maybe (throwError err) return
