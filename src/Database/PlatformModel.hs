{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database.PlatformModel where

--The database models for the cabal data
--For global packages ie packages not localised to a particular platform

import Database.Persist
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)

import Data.Maybe(fromJust)

import Cabal.Conditional(PlatformConditional,wrapPlatformConditionalType,unwrapPlatformConditionalType)
import qualified Cabal.Package as P
import qualified Database.Fields as Field(Version,PackageName,Dependency,PlatformConditionalType,FlagName)

import qualified Distribution.PackageDescription as PD

import Database.GlobalModels


