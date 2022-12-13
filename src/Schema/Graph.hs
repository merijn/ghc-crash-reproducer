{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Schema.Graph where

import Data.Text (Text)
import Database.Persist.TH (persistUpperCase)

import Schema.Utils (mkEntitiesWith)

import Schema.Dataset (DatasetId)
import qualified Schema.Dataset as Dataset

mkEntitiesWith Dataset.schema "schema" [persistUpperCase|
Graph
    name Text
    path Text
    prettyName Text Maybe
    datasetId DatasetId
    UniqGraph path
    UniqGraphName name datasetId
    deriving Eq Show
|]
