module Model.Credentials
  ( Password(..)
  , Credentials(..)
  ) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Model.User (Email)

-- Password
newtype Password = Password
  { unPassword :: String
  } deriving (Show, Eq, Read, FromJSON)

-- Credentials
data Credentials = Credentials
  { credentialsEmail :: Email
  , credentialsPassword :: Password
  } deriving (Show, Eq, Generic)

instance FromJSON Credentials
