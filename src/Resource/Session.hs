module Resource.Session where

import Auth (Session(Session))
import Control.Monad.Trans.Reader (ask)
import Crypto.KDF.BCrypt (validatePassword)
import qualified Data.ByteString.Char8 as B
import Servant
       ((:<|>)(..), (:>), Delete, JSON, NoContent(..), Post, ReqBody,
        ServerT, err401, throwError)
import Servant.Server.Experimental.Auth.Cookie
       (Cookied, addSession, removeSession)

import Database.Users (getByEmail)
import Model.Credentials (Credentials(..), unPassword)
import Model.User (User(..), unHashedPassword)
import qualified Types as T
import Types.Entity (Entity(..))

type PostSessionR
   = "session"
     :> ReqBody '[ JSON] Credentials
     :> Post '[ JSON] (Cookied ())

postSession :: Credentials -> T.App (Cookied ())
postSession Credentials {..} = do
  user <- T.runDB $ getByEmail credentialsEmail
  T.AppContext {..} <- ask
  case user of
    Just (Entity userId User {..}) ->
      let hash = unHashedPassword userHashedPassword
          valid = validatePassword (B.pack $ unPassword credentialsPassword) (B.pack hash)
      in if valid
           then addSession appContextAuthSettings appContextRandomSource appContextServerKey (Session userId) ()
           else throwError err401
    _ -> throwError err401

type DeleteSessionR
   = "session"
     :> Delete '[ JSON] (Cookied NoContent)

deleteSession :: T.App (Cookied NoContent)
deleteSession = do
  T.AppContext {..} <- ask
  removeSession appContextAuthSettings NoContent

type SessionAPI
   = PostSessionR
     :<|> DeleteSessionR

sessionAPI :: ServerT SessionAPI T.App
sessionAPI = postSession :<|> deleteSession
