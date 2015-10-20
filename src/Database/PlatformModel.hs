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

import Cabal.Conditional(FlagConditional
                        ,wrapFlagConditionalType
                        ,unwrapFlagConditionalType)

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
  name String --A globally unique name to identify this package
  UniquePlatformName name
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

--Convert to/from platform models

--Convert to from platform with unique name

toPlatform :: String -> P.Platform -> Platform
toPlatform name platform = Platform {
  platformOs = P.operatingSystem platform,
  platformArch = P.architecture platform,
  platformCompiler = P.compiler platform,
  platformName = name
}

fromPlatform :: Platform -> (String, P.Platform)
fromPlatform platformM = (platformName platformM, platform)
  where platform = P.Platform {
           P.operatingSystem = platformOs platformM,
           P.architecture = platformArch platformM,
           P.compiler = platformCompiler platformM
        }

--Convert a library dependance into a (condition, dependancy pair)
fromPlatformLibraryDep :: PlatformLibraryDependency -> (FlagConditional, Field.Dependency)
fromPlatformLibraryDep libDep = (condition, platformLibraryDependencyDependance libDep)
  where condition = unwrapFlagConditionalType $ platformLibraryDependencyCondition libDep

  --Convert a executable dependance into a (condition, dependancy pair)
fromPlatformExecutableDep :: PlatformExecutableDependency -> (FlagConditional, Field.Dependency)
fromPlatformExecutableDep exeDep = (condition, platformExecutableDependencyDependance exeDep)
  where condition = unwrapFlagConditionalType $ platformExecutableDependencyCondition exeDep
  
--Convert a test dependance into a (condition, dependancy pair)
fromPlatformTestDep :: PlatformTestDependency -> (FlagConditional, Field.Dependency)
fromPlatformTestDep tstDep = (condition, platformTestDependencyDependance tstDep)
  where condition = unwrapFlagConditionalType $ platformTestDependencyCondition tstDep
  
--Convert a benchmark dependance into a (condition, dependancy pair)
fromPlatformBenchmarkDep :: PlatformBenchmarkDependency -> (FlagConditional, Field.Dependency)
fromPlatformBenchmarkDep benDep = (condition, platformBenchmarkDependencyDependance benDep)
  where condition = unwrapFlagConditionalType $ platformBenchmarkDependencyCondition benDep

--Convert a library id and (condition, dependancy pair) into a library dependence
toPlatformLibraryDep :: PlatformLibraryId -> (FlagConditional, Field.Dependency) -> PlatformLibraryDependency
toPlatformLibraryDep libId (conditional, dependency) 
       = PlatformLibraryDependency libId (wrapFlagConditionalType conditional) dependency
       
--Convert a executable id and (condition, dependancy pair) into a executable dependence
toPlatformExecutableDep :: PlatformExecutableId -> (FlagConditional, Field.Dependency) -> PlatformExecutableDependency
toPlatformExecutableDep exeId (conditional, dependency) 
       = PlatformExecutableDependency exeId (wrapFlagConditionalType conditional) dependency
       
--Convert a test id and (condition, dependancy pair) into a test dependence
toPlatformTestDep :: PlatformTestId -> (FlagConditional, Field.Dependency) -> PlatformTestDependency
toPlatformTestDep tstId (conditional, dependency) 
       = PlatformTestDependency tstId (wrapFlagConditionalType conditional) dependency
       
--Convert a benchmark id and (condition, dependancy pair) into a benchmark dependence
toPlatformBenchmarkDep :: PlatformBenchmarkId -> (FlagConditional, Field.Dependency) -> PlatformBenchmarkDependency
toPlatformBenchmarkDep benId (conditional, dependency) 
       = PlatformBenchmarkDependency benId (wrapFlagConditionalType conditional) dependency

--Querys for getting a platform
--Looks up using the unique platform name
getPlatform name = do platformEntity <- fmap fromJust . getBy $ UniquePlatformName name
                      return . snd . fromPlatform $ entityVal platformEntity
--Insert platform with given unique name       
insertPlatform name platform = insert $ toPlatform name platform


                      