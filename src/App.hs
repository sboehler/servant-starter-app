module App
  ( startApp
  ) where

import Database (initializeDatabase)
import Network.Wai.Handler.Warp (run)
import Resource (API, proxy, routes)
import Servant
  ( Application
  , Context((:.), EmptyContext)
  , Proxy(Proxy)
  , Server
  , hoistServer
  , hoistServerWithContext
  , serveWithContext
  )
import Servant.Auth.Server
  ( CookieSettings
  , IsSecure(NotSecure)
  , JWTSettings
  , cookieIsSecure
  , def
  , defaultJWTSettings
  , generateKey
  )
import Types (AppContext(..), convert)

startApp :: IO ()
startApp = do
  myKey <- generateKey
  pool <- initializeDatabase
  -- in production, the cookie should be secure
  let context =
        AppContext
          { appContextPool = pool
          , appContextPort = 4000
          , appContextApproot = "localhost"
          , appContextCookieSettings = def {cookieIsSecure = NotSecure}
          , appContextJWTSettings = defaultJWTSettings myKey
          }
  run (appContextPort context) $ createApp context

createApp :: AppContext -> Application
createApp appContext@AppContext {..} =
  let context = (appContextCookieSettings :. appContextJWTSettings :. EmptyContext)
   in serveWithContext proxy context $ createServer appContext

createServer :: AppContext -> Server API
createServer appContext = hoistServerWithContext proxy context (convert appContext) routes

context :: Proxy '[ CookieSettings, JWTSettings]
context = Proxy
