{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database.Fields(Version,PackageName,
                       PlatformConditionalType,
                       VersionRange,Dependency) where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)

import Cabal.Conditional(PlatformConditionalType)
import Distribution.Package(PackageName, Dependency)
import Distribution.Version(VersionRange)
import Data.Version

--Custom fields to be used in persistence
derivePersistField "Version"
derivePersistField "VersionRange"
derivePersistField "Dependency"
derivePersistField "PackageName"

--Global conditional for a generic dependency without platform or flags specified
derivePersistField "PlatformConditionalType"