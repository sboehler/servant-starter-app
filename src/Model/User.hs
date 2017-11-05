module Model.User
  ( User(..)
  , HashedPassword(..)
  , Email(..)
  , UserId
  ) where

import Data.Aeson (FromJSON, ToJSON, (.=), object, toJSON)
import Data.Serialize (Serialize)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))
import GHC.Generics (Generic)

import Types.Entity (Id)

-- UserId
newtype UserId =
  UserId Int
  deriving (Eq, Show, Read, Generic, FromField, ToField, FromJSON, ToJSON)

instance Serialize UserId

-- Email
newtype Email = Email
  { unEmail :: String
  } deriving (Show, Eq, Read, FromField, ToField, Generic, FromJSON, ToJSON)

-- HashedPassword
newtype HashedPassword = HashedPassword
  { unHashedPassword :: String
  } deriving (Show, Eq, Read, FromField, ToField)

-- User
data User = User
  { userEmail :: Email
  , userHashedPassword :: HashedPassword
  } deriving (Show, Eq, Generic)

instance ToJSON User where
  toJSON User {..} = object ["email" .= userEmail]

instance FromRow User

type instance Id User = UserId
