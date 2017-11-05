module App
  ( startApp
  ) where

import Auth (cookieAuthCheck, mkAuthSettings)
import Crypto.Random (drgNew)
import Database (initializeDatabase)
import Network.Wai.Handler.Warp (run)
import Resource (API, proxy, routes)
import Servant
       (Application, Context((:.), EmptyContext), Server, enter,
        serveWithContext)
import Servant.Server.Experimental.Auth.Cookie
       (mkPersistentServerKey, mkRandomSource)
import Types (AppContext(..), convert)

startApp :: IO ()
startApp = do
  randomSource <- mkRandomSource drgNew 2000
  let serverKey = mkPersistentServerKey "provide a secret key here"
  pool <- initializeDatabase
  let authSettings = mkAuthSettings
  let context =
        AppContext
        { appContextPool = pool
        , appContextPort = 4000
        , appContextRandomSource = randomSource
        , appContextServerKey = serverKey
        , appContextScheme = "http:"
        , appContextAuthSettings = authSettings
        , appContextApproot = "localhost"
        }
  run (appContextPort context) $ app context

app :: AppContext -> Application
app appContext@AppContext {..} =
  let context = (cookieAuthCheck appContext :. EmptyContext)
  in serveWithContext proxy context $ server appContext

server :: AppContext -> Server API
server context = enter (convert context) routes
