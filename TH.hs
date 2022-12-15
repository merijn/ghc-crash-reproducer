{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module TH (templateFoo) where

import Language.Haskell.TH (Dec, Q)
import Language.Haskell.TH.Syntax (liftString)

import CAPI

templateFoo :: String -> Q [Dec]
templateFoo name = [d|str :: String;str = $(liftString name)|]
