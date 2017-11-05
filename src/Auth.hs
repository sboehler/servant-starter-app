module Auth
  ( cookieAuthCheck
  , mkAuthSettings
  , AppAuth
  , Session(..)
  ) where

import Control.Monad.Catch (try)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (ctrCombine)
import Crypto.Hash.Algorithms (SHA256)
import Data.Aeson (ToJSON)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Network.Wai (Request)
import Servant (AuthProtect, Proxy(Proxy), err401, throwError)
import Servant.Server.Experimental.Auth
       (AuthHandler, mkAuthHandler)
import Servant.Server.Experimental.Auth.Cookie
       (AuthCookieData, AuthCookieException, AuthCookieSettings(..),
        AuthCookieSettings, WithMetadata(..), WithMetadata, getSession)

import Model.User (UserId)
import Types (AppContext(..))

newtype Session =
  Session UserId
  deriving (Show, Eq, Generic)

instance ToJSON Session

instance Serialize Session

type instance AuthCookieData = Session

cookieAuthCheck :: AppContext -> AuthHandler Request (WithMetadata Session)
cookieAuthCheck AppContext {..} =
  mkAuthHandler $ \request -> do
    result <- try (getSession appContextAuthSettings appContextServerKey request)
    case result :: Either AuthCookieException (Maybe (WithMetadata Session)) of
      Right (Just meta) -> return meta
      _ -> throwError err401

type AppAuth = AuthProtect "cookie-auth"

mkAuthSettings :: AuthCookieSettings
mkAuthSettings =
  AuthCookieSettings
  { acsSessionField = "Session"
  , acsCookieFlags = ["HttpOnly"]
  , acsMaxAge = fromIntegral (6 * 3600 :: Integer)
  , acsExpirationFormat = "%0Y%m%d%H%M%S"
  , acsPath = "/"
  , acsHashAlgorithm = Proxy :: Proxy SHA256
  , acsCipher = Proxy :: Proxy AES256
  , acsEncryptAlgorithm = ctrCombine
  , acsDecryptAlgorithm = ctrCombine
  }
