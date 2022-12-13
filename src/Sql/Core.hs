{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Sql.Core
    ( MonadSql
    , executeSql
    , executeSqlSingleValueMaybe
    , executeSqlSingleValue
    , Transaction(Transaction)
    , SqlRecord
    , runMigrationQuiet
    , MonadLogger
    , MonadThrow
    ) where

import Control.Monad.Catch (throwM, MonadCatch, MonadThrow, MonadMask, Exception(..))
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO(..))
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (ReaderT, withReaderT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Database.Persist.Sqlite
    ( BackendCompatible
    , Migration
    , PersistField
    , PersistRecordBackend
    , RawSqlite
    , Single(Single)
    , SqlBackend
    )
import qualified Database.Persist.Sqlite as Sqlite

-- Important! This is what breaks!
import SQLiteExts

data AbortTransaction = AbortTransaction Text
    deriving (Show, Typeable)

instance Exception AbortTransaction

data PatternFailed = PatternFailed Text
    deriving (Show, Typeable)

instance Exception PatternFailed

data ExpectedSingleValue = ExpectedSingleValue Text
    deriving (Show, Typeable)

instance Exception ExpectedSingleValue

data QueryReturnedZeroResults = QueryReturnedZeroResults
    deriving (Show, Typeable)

instance Exception QueryReturnedZeroResults

newtype Avg = Avg { getAvg :: Int } deriving (Show, Eq, Ord)
newtype Max = Max { getMax :: Int } deriving (Show, Eq, Ord)

class MonadResource m => MonadSql m
instance MonadSql m => MonadSql (ReaderT r m)

newtype Transaction m r = Transaction (ReaderT (RawSqlite SqlBackend) m r)
  deriving
  ( Functor, Applicative, Monad, MonadCatch, MonadFail, MonadIO, MonadLogger
  , MonadResource, MonadThrow, MonadTrans
  )

type SqlRecord rec = (PersistRecordBackend rec (RawSqlite SqlBackend))

liftProjectPersist
    :: (BackendCompatible sup (RawSqlite SqlBackend), MonadSql m)
    => ReaderT sup IO a -> Transaction m a
liftProjectPersist =
    Transaction . Sqlite.liftPersist . withReaderT Sqlite.projectBackend

executeSql :: MonadSql m => Text -> Transaction m ()
executeSql query = Transaction $ Sqlite.rawExecute query []

executeSqlSingleValueMaybe
    :: (MonadLogger m, MonadSql m, MonadThrow m, PersistField a)
    => Text -> Transaction m (Maybe a)
executeSqlSingleValueMaybe query = Transaction $ do
    result <- Sqlite.rawSql query []
    case result of
        [] -> return Nothing
        [Single v] -> return $ Just v
        _ -> throwM $ ExpectedSingleValue query

executeSqlSingleValue
    :: (MonadLogger m, MonadSql m, MonadThrow m, PersistField a)
    => Text -> Transaction m a
executeSqlSingleValue query = do
    result <- executeSqlSingleValueMaybe query
    case result of
        Just v -> return v
        Nothing -> throwM QueryReturnedZeroResults

runMigrationQuiet :: (MonadSql m) => Migration -> Transaction m [Text]
runMigrationQuiet = liftProjectPersist . Sqlite.runMigrationQuiet

type SqlPool = Pool (RawSqlite SqlBackend)

newtype SqlT m a = SqlT { unSqlT :: ReaderT SqlPool m a }
  deriving
  ( Applicative, Functor, Monad, MonadCatch, MonadIO, MonadLogger
  , MonadLoggerIO, MonadMask, MonadResource, MonadThrow)

instance (MonadLogger m, MonadThrow m) => MonadFail (SqlT m) where
    fail = throwM . PatternFailed . T.pack

instance MonadTrans SqlT where
    lift = SqlT . lift

instance MonadUnliftIO m => MonadUnliftIO (SqlT m) where
    withRunInIO inner = SqlT $ withRunInIO $ \run -> inner (run . unSqlT)
    {-# INLINE withRunInIO #-}
