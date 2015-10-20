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
import qualified Cabal.Package as Package
import qualified Database.Fields as Field(OS,Arch,CompilerVersion
                                         ,Dependency, FlagConditionalType)

import qualified Distribution.PackageDescription as PD

import Cabal.Conditional(FlagConditional
                        ,wrapFlagConditionalType
                        ,unwrapFlagConditionalType)

import Database.GlobalModels
import Control.Applicative ((<$>))

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
  globalPackage GlobalPackageId
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

toPlatform :: P.Platform -> Platform
toPlatform platform = Platform {
  platformOs = P.operatingSystem platform,
  platformArch = P.architecture platform,
  platformCompiler = P.compiler platform,
  platformName = P.globalPlatformName platform
}

fromPlatform :: Platform -> P.Platform
fromPlatform platformM = platform
  where platform = P.Platform {
           P.operatingSystem = platformOs platformM,
           P.architecture = platformArch platformM,
           P.compiler = platformCompiler platformM,
           P.globalPlatformName = platformName platformM
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
                      return . fromPlatform $ entityVal platformEntity
--Insert platform with given unique name  
--Do not insert a duplicate in this case return the old key     
insertPlatform platform = do eitherKey <- insertBy $ toPlatform platform
                             case eitherKey of
                                 Left exists -> return (entityKey exists)  
                                 Right newKey -> return newKey       


--Query for getting a package specified down to a package
--Takes the platform name and the name/version pair of the package
getPlatformPackage platformName name version
       = do platformEntity <- fmap fromJust . getBy $ UniquePlatformName platformName
            globalPkgEntity <- fmap fromJust . getBy $ PackageIdentifier name version
            let platformId = entityKey platformEntity
                globalPkgId = entityKey globalPkgEntity
            --Get package from the global package and platform
            packageEntity <- fmap fromJust . getBy $ PlatformPackageIdentifier platformId globalPkgId
            let packageKey = entityKey packageEntity
            --Get global package data
            globalPackageData <- fmap fromJust . get . globalPackagePackageData $ entityVal globalPkgEntity
            --Get build targets
            libraryM <- getPlatformLibrary packageKey
            --Get executable build targets
            executableEntities <- selectList [PlatformExecutablePackage ==. packageKey] []
            executables <- mapM getPlatformExecutable executableEntities
            --Get test build targets
            testEntities <- selectList [PlatformTestPackage ==. packageKey] []
            tests <- mapM getPlatformTest testEntities
            --Get benchmark build targets
            benchmarkEntities <- selectList [PlatformBenchmarkPackage ==. packageKey] []
            benchmarks <- mapM getPlatformBenchmark benchmarkEntities
            --Get flags
            flagEnities <- selectList [FlagPackage ==. globalPkgId] []
            let flags  = map (fromFlag . entityVal) flagEnities
            --First the 
            return Package.Package {
               Package.globalProperties = toGlobalPackageData name version globalPackageData,
               Package.library = libraryM,
               Package.executables = executables,
               Package.tests = tests,
               Package.benchmarks = benchmarks,
               Package.flags = flags
            }
  where getPlatformLibrary packageKey =
            do libraryEntityM <- getBy $ PlatformLibraryUniquePackage packageKey
               return Nothing
               case libraryEntityM of
                  Nothing -> return Nothing
                  Just libraryEntity -> do --Get dependencies
                                           let libraryKey = entityKey libraryEntity
                                           dependencyEntities <- selectList [PlatformLibraryDependencyLibrary ==. libraryKey] []
                                           let dependencies = map (fromPlatformLibraryDep . entityVal) dependencyEntities
                                           return $ Just Package.Library {
                                             Package.libraryBuildDependencies = dependencies
                                           }
        getPlatformExecutable executableEntity =
            do let executableKey = entityKey executableEntity
                   globalExecutableKey = platformExecutableGlobalExecutable $ entityVal executableEntity
               globalExecutableEntity <- fromJust <$> get globalExecutableKey
               let executableName = globalExecutableName globalExecutableEntity
               dependencyEntities <- selectList [PlatformExecutableDependencyExecutable ==. executableKey] []
               let dependencies = map (fromPlatformExecutableDep . entityVal) dependencyEntities
               return Package.Executable {
                 Package.executableTargetName = executableName,
                 Package.executableBuildDependencies = dependencies
               }
        getPlatformTest testEntity =
            do let testKey = entityKey testEntity
                   globalTestKey = platformTestGlobalTest $ entityVal testEntity
               globalTestEntity <- fromJust <$> get globalTestKey
               let testName = globalTestName globalTestEntity
               dependencyEntities <- selectList [PlatformTestDependencyTest ==. testKey] []
               let dependencies = map (fromPlatformTestDep . entityVal) dependencyEntities
               return Package.TestSuite {
                 Package.testTargetName = testName,
                 Package.testBuildDependencies = dependencies
               }
        getPlatformBenchmark benchmarkEntity =
            do let benchmarkKey = entityKey benchmarkEntity
                   globalBenchmarkKey = platformBenchmarkGlobalBenchmark $ entityVal benchmarkEntity
               globalBenchmarkEntity <- fromJust <$> get globalBenchmarkKey
               let benchmarkName = globalBenchmarkName globalBenchmarkEntity
               dependencyEntities <- selectList [PlatformBenchmarkDependencyBenchmark ==. benchmarkKey] []
               let dependencies = map (fromPlatformBenchmarkDep . entityVal) dependencyEntities
               return Package.Benchmark {
                 Package.benchmarkTargetName = benchmarkName,
                 Package.benchmarkBuildDependencies = dependencies
               }
                      
