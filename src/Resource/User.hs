module Resource.User
  ( UserAPI
  , userAPI
  ) where

import Control.Monad.IO.Class (liftIO)
import Crypto.KDF.BCrypt (hashPassword)
import qualified Data.ByteString.Char8 as B
import Servant
  ( (:<|>)((:<|>))
  , (:>)
  , Get
  , JSON
  , Post
  , ReqBody
  , ServerT
  , err400
  , err401
  , err404
  , err500
  , throwError
  )
import Servant.Auth.Server (Auth, AuthResult(Authenticated), Cookie)

import Database.Users (get, getByEmail, insert)
import Model.Credentials (Credentials(..), unPassword)
import Model.User (HashedPassword(..), User(..))
import Resource.Session (Session(..))
import Resource.Utils (orElseThrow)
import Types (App, runDB)
import Types.Entity (Entity)

type GetUserR
   = Auth '[ Cookie] Session
     :> "user"
     :> Get '[ JSON] (Entity User)

getUser :: AuthResult Session -> App (Entity User)
getUser (Authenticated (Session userId)) = runDB (get userId) >>= orElseThrow err404
getUser _ = throwError err401

type PostUserR
   = "user"
     :> ReqBody '[ JSON] Credentials
     :> Post '[ JSON] (Entity User)

postUser :: Credentials -> App (Entity User)
postUser Credentials {..} = do
  existingUser <- runDB $ getByEmail credentialsEmail
  case existingUser of
    Just _ -> throwError err400
    Nothing -> do
      hashedPassword <- liftIO $ (hashPassword 12 . B.pack . unPassword) credentialsPassword
      let user = User credentialsEmail (HashedPassword $ B.unpack hashedPassword)
      runDB (insert user) >>= orElseThrow err500

type UserAPI
   = GetUserR
     :<|> PostUserR

userAPI :: ServerT UserAPI App
userAPI = getUser :<|> postUser
