module Database
  ( initializeDatabase
  , Connection
  , runQuery
  , runQuery_
  , Fetch
  ) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.ByteString.Char8 as BS8 (pack)
import Data.Pool (Pool, createPool, withResource)
import Database.PostgreSQL.Simple
       (Connection, FromRow, Query, close, connectPostgreSQL, query,
        query_, withTransaction)
import Database.PostgreSQL.Simple.Migration
       (MigrationCommand(MigrationDirectory, MigrationInitialization),
        MigrationContext(MigrationContext), runMigration)
import Database.PostgreSQL.Simple.ToRow (ToRow)

getDbConnection :: IO Connection
getDbConnection = connectPostgreSQL (BS8.pack url)
  where
    url = "dbname=servant-starter-app"

closeDbConnection :: Connection -> IO ()
closeDbConnection = close

initializeDatabase :: IO (Pool Connection)
initializeDatabase = do
  pool <- createPool getDbConnection closeDbConnection 1 10 10
  let dir = "config/schema"
  withResource pool $ \con -> do
    _ <- withTransaction con $ runMigration $ MigrationContext MigrationInitialization True con
    _ <- withTransaction con $ runMigration $ MigrationContext (MigrationDirectory dir) True con
    return ()
  return pool

type Fetch a = ReaderT Connection (MaybeT IO) a

runQuery_ :: FromRow a => Query -> Fetch [a]
runQuery_ q = do
  con <- ask
  liftIO $ query_ con q

runQuery :: (FromRow a, ToRow q) => Query -> q -> Fetch [a]
runQuery q a = do
  con <- ask
  liftIO $ query con q a
