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

import qualified Cabal.Platform as P
import qualified Database.Fields as Field(OS,Arch,CompilerVersion
                                         ,Dependency, FlagConditionalType)

import qualified Distribution.PackageDescription as PD

import Database.GlobalModels

--Construct the database models for the platform dependent data structures used in the cabal models
--Add interfaces between them

mkPersist sqlSettings [persistLowerCase|
--The data associated to a particular platform that a package can
-- be built on
Platform
  os Field.OS
  arch Field.Arch
  compiler Field.CompilerVersion
--Localise a package to a particular platform
--Contains links back to the relevent global package and has links from 
--Localised build targets
PlatformPackage
  platform PlatformId
  globalPackage GlobalPackage
  PlatformPackageIdentifier platform globalPackage --The combination of package 
                                                   --And platform should be unique.
--Build targets and dependencies
--A library localised to a particular platform but without flags being set
--Used to be linked to dependencies
PlatformLibrary
  package PlatformPackageId
  globalLibrary GlobalLibraryId --Related global library
  PlatformLibraryUniquePackage package --Unique as platform can have at most 1 library
PlatformLibraryDependency
  library PlatformLibraryId
  condition Field.FlagConditionalType
  dependance Field.Dependency
--A executable localised to a particular platform but without flags being set
--Used to be linked to dependencies
PlatformExecutable
  package PlatformPackageId
  globalExecutable GlobalExecutableId --Related global executable
PlatformExecutableDependency
  executable PlatformExecutableId
  condition Field.FlagConditionalType
  dependance Field.Dependency
--A test localised to a particular platform but without flags being set
--Used to be linked to dependencies
PlatformTest
  package PlatformPackageId
  globalTest GlobalTestId --Related global test
PlatformTestDependency
  test PlatformTestId
  condition Field.FlagConditionalType
  dependance Field.Dependency
--A benchmark localised to a particular platform but without flags being set
--Used to be linked to dependencies
PlatformBenchmark
  package PlatformPackageId
  globalBenchmark GlobalBenchmarkId --Related global benchmark
PlatformBenchmarkDependency
  benchmark PlatformBenchmarkId
  condition Field.FlagConditionalType
  dependance Field.Dependency
|]