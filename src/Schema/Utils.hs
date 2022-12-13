{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Schema.Utils
    ( MonadLogger
    , MonadSql
    , MonadThrow
    , Transaction
    , Entity
    , EntityDef
    , ForeignDef
    , Int64
    , (.=)
    , (.>)
    , Sql.executeSql
    , Sql.executeSqlSingleValue
    , Sql.executeSqlSingleValueMaybe
    , createTableFromSchema
    , mkForeignRef
    , addForeignRef
    , mkEntities
    , mkMigration
    , mkMigrationLookup
    , getTypeName
    ) where

import Control.Monad (void)
import Data.Int (Int64)
import qualified Data.Map as M
import Data.Text (Text)
import Database.Persist.EntityDef.Internal (EntityDef(..))
import Database.Persist.Sql (Entity, Migration, entityDef)
import Database.Persist.TH (migrateModels)
import Database.Persist.Types
    ( ConstraintNameDB(..)
    , ConstraintNameHS(..)
    , EntityNameDB(..)
    , EntityNameHS(..)
    , FieldNameDB(..)
    , FieldNameHS(..)
    , ForeignDef(..)
    , noCascade)

import Schema.Utils.TH (mkEntities)
import Sql.Core (MonadLogger, MonadSql, MonadThrow, SqlRecord, Transaction)
import qualified Sql.Core as Sql

(.=) :: Applicative f => a -> b -> (a, (b, f ()))
(.=) i schema = (i, (schema, pure ()))

(.>) :: Functor f => a -> b -> f c -> (a, (b, f c))
(.>) i schema act = (i, (schema, act))

mkMigration :: [[EntityDef]] -> Migration
mkMigration = migrateModels . concat

createTableFromSchema :: MonadSql m => [EntityDef] -> Transaction m ()
createTableFromSchema = void . Sql.runMigrationQuiet . mkMigration . pure

mkForeignRef :: Text -> [(Text, Text)] -> ForeignDef
mkForeignRef foreignTable refs = ForeignDef
    { foreignRefTableHaskell = EntityNameHS foreignTable
    , foreignRefTableDBName = EntityNameDB foreignTable
    , foreignConstraintNameHaskell =
        ConstraintNameHS $ "Foreign" <> foreignTable
    , foreignConstraintNameDBName =
        ConstraintNameDB $ "Foreign" <> foreignTable
    , foreignFields = map wrapNames refs
    , foreignFieldCascade = noCascade
    , foreignToPrimary = False
    , foreignAttrs = []
    , foreignNullable = False
    }
  where
    wrapNames
        :: (Text, Text)
        -> ((FieldNameHS, FieldNameDB), (FieldNameHS, FieldNameDB))
    wrapNames (src, dst) =
      ((FieldNameHS src, FieldNameDB src), (FieldNameHS dst, FieldNameDB dst))

addForeignRef :: Text -> ForeignDef -> [EntityDef] -> [EntityDef]
addForeignRef _ _ [] = []
addForeignRef name fk (ent:ents)
    | entityHaskell ent /= EntityNameHS name = ent : addForeignRef name fk ents
    | otherwise = ent { entityForeigns = fk : entityForeigns ent } : ents

mkMigrationLookup
    :: MonadSql m
    => [(Int64, ([EntityDef], Transaction m ()))]
    -> Int64
    -> Transaction m [EntityDef]
mkMigrationLookup (M.fromList -> migrationMap) = \i ->
    case M.lookupLE i migrationMap of
        Nothing -> return []
        Just (key, (schema, migration))
            | key == i -> schema <$ migration
            | otherwise -> return schema

getTypeName :: SqlRecord rec => proxy rec -> Text
getTypeName = unEntityNameHS . entityHaskell . entityDef
