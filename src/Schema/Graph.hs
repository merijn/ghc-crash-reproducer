{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Graph where

import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist.Sql (Unique)
import Database.Persist.TH (persistUpperCase)

import Schema.Utils
    (Entity, EntityDef, Int64, MonadSql, Transaction, (.>), (.=))
import qualified Schema.Utils as Utils

import Schema.Dataset (DatasetId)
import qualified Schema.Dataset as Dataset
import qualified Schema.Graph.V0 as V0
import qualified Schema.Graph.V1 as V1

Utils.mkEntitiesWith Dataset.schema "schema" [persistUpperCase|
Graph
    name Text
    path Text
    prettyName Text Maybe
    datasetId DatasetId
    timestamp UTCTime
    UniqGraph path
    UniqGraphName name datasetId
    deriving Eq Show
|]

deriving instance Show (Unique Graph)

migrations :: MonadSql m => Int64 -> Transaction m [EntityDef]
migrations = Utils.mkMigrationLookup
    [ 1 .> V0.schema $ do
        Utils.executeSql [i|
ALTER TABLE "Graph"
ADD COLUMN "dataset" VARCHAR NOT NULL DEFAULT 'unknown'
|]
    , 5 .= V1.schema
    , 20 .> schema $ do
        Utils.executeSql [i|
ALTER TABLE "Graph"
ADD COLUMN "timestamp" TIMESTAMP
|]

        Utils.executeSql [i|
REPLACE INTO "Graph"
SELECT Graph.id
     , Graph.name
     , Graph.path
     , Graph.prettyName
     , Graph.datasetId
     , IFNULL(TimeStamp.minTime, strftime('%Y-%m-%dT%H:%M:%f', 'now'))
FROM Graph

LEFT JOIN
(   SELECT Graph.id, MIN(Run.timestamp) AS minTime
    FROM Graph

    INNER JOIN Variant
    ON Graph.id = Variant.graphId

    INNER JOIN Run
    ON Variant.id = Run.variantId

    GROUP BY Graph.id
) AS TimeStamp
ON Graph.id = TimeStamp.id
|]
    ]
