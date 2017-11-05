module Resource
  ( API
  , proxy
  , routes
  ) where

import Resource.Session (SessionAPI, sessionAPI)
import Resource.User (UserAPI, userAPI)
import Servant ((:<|>)(..), Proxy(Proxy), ServerT)
import Types (App)

type API
   = SessionAPI
     :<|> UserAPI

routes :: ServerT API App
routes = sessionAPI :<|> userAPI

proxy :: Proxy API
proxy = Proxy
