module App
  ( startApp
  ) where

import Database (initializeDatabase)
import Network.Wai.Handler.Warp (run)
import Resource (API, proxy, routes)
import Servant
       (Application, Context((:.), EmptyContext), Server, enter,
        serveWithContext)
import Servant.Auth.Server
       (IsSecure(NotSecure), cookieIsSecure, def, defaultJWTSettings,
        generateKey)
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
  run (appContextPort context) $ app context

app :: AppContext -> Application
app appContext@AppContext {..} =
  let context = (appContextCookieSettings :. appContextJWTSettings :. EmptyContext)
  in serveWithContext proxy context $ server appContext

server :: AppContext -> Server API
server context = enter (convert context) routes
