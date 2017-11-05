module Types.Entity
  ( Entity(..)
  , Id
  ) where

import Data.Aeson (ToJSON, (.=), object, toJSON)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)

-- A type family for database ids
type family Id a

-- A wrapper for models
data Entity model = Entity
  { entityId :: Id model
  , entityModel :: model
  }

deriving instance
         (Show (Id model), Show model) => Show (Entity model)

instance (Eq (Id model)) => Eq (Entity model) where
  e1 == e2 = entityId e1 == entityId e2

instance (ToField (Id model), ToRow model) => ToRow (Entity model) where
  toRow (Entity i m) = toField i : toRow m

instance (FromField (Id model), FromRow model) => FromRow (Entity model) where
  fromRow = Entity <$> field <*> fromRow

instance (ToJSON (Id model), ToJSON model) => ToJSON (Entity model) where
  toJSON e = object ["id" .= toJSON (entityId e), "model" .= toJSON (entityModel e)]
