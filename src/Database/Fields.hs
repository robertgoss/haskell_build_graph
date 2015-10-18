{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database.Fields(Version,PackageName,
                       PlatformConditionalType,
                       FlagConditionalType,
                       VersionRange,Dependency,
                       FlagName,
                       OS,Arch,
                       CompilerVersion) where

import Database.Persist
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)

import Cabal.Conditional(PlatformConditionalType,FlagConditionalType)
import Cabal.Conditional(CompilerVersion)

import Distribution.Package(PackageName, Dependency)
import Distribution.PackageDescription(FlagName)
import Distribution.Version(VersionRange)
import Data.Version
import Distribution.System(OS,Arch)


--Custom fields to be used in persistence
--Re export cabal structures with standard show/read
derivePersistField "Version"
derivePersistField "VersionRange"
derivePersistField "Dependency"
derivePersistField "PackageName"
derivePersistField "FlagName"
derivePersistField "OS"
derivePersistField "Arch"

--Global conditional for a generic dependency without platform or flags specified
derivePersistField "PlatformConditionalType"

--Platform conditional for a platform localised dependency without flags specified
derivePersistField "FlagConditionalType"

--Compiler version using default show/read
derivePersistField "CompilerVersion"