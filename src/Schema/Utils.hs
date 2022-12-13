{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Schema.Utils (mkEntities, mkEntitiesWith) where

import Language.Haskell.TH (Dec, Q)
import Database.Persist.EntityDef (EntityDef)
import Database.Persist.Quasi.Internal (UnboundEntityDef)
import qualified Database.Persist.TH as TH

import SQLiteExts

mkEntities :: String -> [UnboundEntityDef] -> Q [Dec]
mkEntities name = TH.share
    [TH.mkPersist TH.sqlSettings, TH.mkEntityDefList name]

mkEntitiesWith :: [EntityDef] -> String -> [UnboundEntityDef] -> Q [Dec]
mkEntitiesWith ents name = TH.share
    [TH.mkPersistWith TH.sqlSettings ents, TH.mkEntityDefList name]
