{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database.GlobalModels where

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


--Construct the database models for the global data structures used in the cabal models
--Add interfaces between them

mkPersist sqlSettings [persistLowerCase|
--Global package data a gloss over the package version
--With name and version moved to package
GlobalPackageData
    synopsis String Maybe
    description String Maybe
    category String Maybe
    stability String Maybe
    license String
    copyright String Maybe
    author String Maybe
    maintainer String Maybe
    homepage String Maybe
    bugReports String Maybe
    packageUrl String Maybe
    deriving Show
--A global package with no platform localisation or flags set.
--Motly has inverse maps for things
GlobalPackage
    name Field.PackageName
    version Field.Version
    packageData GlobalPackageDataId --Associated package data
    PackageIdentifier name version --Unique identifier for a package with name and version
--A global version of a library with no platform localisation or flags set.
--Has inverse maps for dependencies
GlobalLibrary
    package GlobalPackageId
    GlobalLibraryUniquePackage package --The package should be unique
GlobalLibraryDependency
    library GlobalLibraryId
    condition Field.PlatformConditionalType
    dependance Field.Dependency
--A global version of a executable with no platform localisation or flags set.
--Has inverse maps for dependencies
GlobalExecutable
    name String
    package GlobalPackageId
GlobalExecutableDependency
    executable GlobalExecutableId
    condition Field.PlatformConditionalType
    dependance Field.Dependency
--A global version of a test suite with no platform localisation or flags set.
--Has inverse maps for dependencies
GlobalTest
    name String
    package GlobalPackageId
GlobalTestDependency
    test GlobalTestId
    condition Field.PlatformConditionalType
    dependance Field.Dependency
--A global version of a benchmark with no platform localisation or flags set.
--Has inverse maps for dependencies
GlobalBenchmark
    name String
    package GlobalPackageId
GlobalBenchmarkDependency
    benchmark GlobalBenchmarkId
    condition Field.PlatformConditionalType
    dependance Field.Dependency
--Flag for a configuration
Flag
    package GlobalPackageId --Package this flag is attached to
    name Field.FlagName --name of the flag 
    description String
    default Bool
    manual Bool
|]

--


--Convert to/from the cabal modules

fromGlobalPackageDataModel :: P.GlobalPackageData -> GlobalPackageData
fromGlobalPackageDataModel globalPackageData = GlobalPackageData {
  globalPackageDataSynopsis = P.synopsis globalPackageData,
  globalPackageDataDescription = P.description globalPackageData,
  globalPackageDataCategory = P.category globalPackageData,
  globalPackageDataStability = P.stability globalPackageData,
  globalPackageDataLicense = P.license globalPackageData,
  globalPackageDataCopyright = P.copyright globalPackageData,
  globalPackageDataAuthor = P.author globalPackageData,
  globalPackageDataMaintainer = P.maintainer globalPackageData,
  globalPackageDataHomepage = P.homepage globalPackageData,
  globalPackageDataBugReports = P.bugReports globalPackageData,
  globalPackageDataPackageUrl = P.packageUrl globalPackageData
}

toGlobalPackageData :: Field.PackageName -> Field.Version -> GlobalPackageData -> P.GlobalPackageData
toGlobalPackageData name version globalPackageDataModel = P.GlobalPackageData {
  P.name = name,
  P.version = version,
  P.synopsis = globalPackageDataSynopsis globalPackageDataModel,
  P.description = globalPackageDataDescription globalPackageDataModel,
  P.category = globalPackageDataCategory globalPackageDataModel,
  P.stability = globalPackageDataStability globalPackageDataModel,
  P.license = globalPackageDataLicense globalPackageDataModel,
  P.copyright = globalPackageDataCopyright globalPackageDataModel,
  P.author = globalPackageDataAuthor globalPackageDataModel,
  P.maintainer = globalPackageDataMaintainer globalPackageDataModel,
  P.homepage = globalPackageDataHomepage globalPackageDataModel,
  P.bugReports = globalPackageDataBugReports globalPackageDataModel,
  P.packageUrl = globalPackageDataPackageUrl globalPackageDataModel
}

--Convert a library dependance into a (condition, dependancy pair)
fromLibraryDep :: GlobalLibraryDependency -> (PlatformConditional, Field.Dependency)
fromLibraryDep libDep = (condition, globalLibraryDependencyDependance libDep)
  where condition = unwrapPlatformConditionalType $ globalLibraryDependencyCondition libDep

--Convert a executable dependance into a (condition, dependancy pair)
fromExecutableDep :: GlobalExecutableDependency -> (PlatformConditional, Field.Dependency)
fromExecutableDep exeDep = (condition, globalExecutableDependencyDependance exeDep)
  where condition = unwrapPlatformConditionalType $ globalExecutableDependencyCondition exeDep

--Convert a test dependance into a (condition, dependancy pair)
fromTestDep :: GlobalTestDependency -> (PlatformConditional, Field.Dependency)
fromTestDep testDep = (condition, globalTestDependencyDependance testDep)
  where condition = unwrapPlatformConditionalType $ globalTestDependencyCondition testDep

--Convert a benchmark dependance into a (condition, dependancy pair)
fromBenchmarkDep :: GlobalBenchmarkDependency -> (PlatformConditional, Field.Dependency)
fromBenchmarkDep benDep = (condition, globalBenchmarkDependencyDependance benDep)
  where condition = unwrapPlatformConditionalType $ globalBenchmarkDependencyCondition benDep

--Convert a library id and (condition, dependancy pair) into a library dependence
toLibraryDep :: GlobalLibraryId -> (PlatformConditional, Field.Dependency) -> GlobalLibraryDependency
toLibraryDep libId (conditional, dependency) = GlobalLibraryDependency libId (wrapPlatformConditionalType conditional) dependency

--Convert a library id and (condition, dependancy pair) into a library dependence
toExecutableDep :: GlobalExecutableId -> (PlatformConditional, Field.Dependency) -> GlobalExecutableDependency
toExecutableDep exeId (conditional, dependency) = GlobalExecutableDependency exeId (wrapPlatformConditionalType conditional) dependency

--Convert a test id and (condition, dependancy pair) into a test dependence
toTestDep :: GlobalTestId -> (PlatformConditional, Field.Dependency) -> GlobalTestDependency
toTestDep testId (conditional, dependency) = GlobalTestDependency testId (wrapPlatformConditionalType conditional) dependency

--Convert a benchmark id and (condition, dependancy pair) into a benchmark dependence
toBenchmarkDep :: GlobalBenchmarkId -> (PlatformConditional, Field.Dependency) -> GlobalBenchmarkDependency
toBenchmarkDep benId (conditional, dependency) = GlobalBenchmarkDependency benId (wrapPlatformConditionalType conditional) dependency

--Convert a flag model  to a package flag
fromFlag :: Flag -> PD.Flag
fromFlag flag = PD.MkFlag {
  PD.flagName = flagName flag,
  PD.flagDescription = flagDescription flag,
  PD.flagDefault = flagDefault flag,
  PD.flagManual = flagManual flag
}

--Queries for getting a global package
--Big set of queries as need many invere maps
getGlobalPackage name version = do --Get package then build targets
                                   pkgEntity <- fmap fromJust . getBy $ PackageIdentifier name version
                                   getGlobalPackageFromId $ entityKey pkgEntity
getGlobalPackageFromId pkgKey = do pkgVal <- fmap fromJust $ get pkgKey
                                   let name = globalPackageName pkgVal
                                       version = globalPackageVersion pkgVal
                                   --Get global data
                                   globalPackageData <- fmap fromJust . get . globalPackagePackageData $ pkgVal
                                   --Get library (maybe)
                                   libraryM <- getGlobalLibrary $ pkgKey
                                   --Get executables
                                   executableEnities <- selectList [GlobalExecutablePackage ==. pkgKey] []
                                   executables <- mapM getGlobalExecutable executableEnities
                                   --Get test
                                   testEnities <- selectList [GlobalTestPackage ==. pkgKey] []
                                   tests <- mapM getGlobalTest testEnities
                                   --Get benchmark
                                   benchmarkEnities <- selectList [GlobalBenchmarkPackage ==. pkgKey] []
                                   benchmarks <- mapM getGlobalBenchmark benchmarkEnities
                                   --Get flags
                                   flagEnities <- selectList [FlagPackage ==. pkgKey] []
                                   let flags  = map (fromFlag . entityVal) flagEnities
                                   --Return package
                                   return P.Package {
                                     P.globalProperties = toGlobalPackageData name version globalPackageData,
                                     P.library = libraryM,
                                     P.executables = executables,
                                     P.tests = tests,
                                     P.benchmarks = benchmarks,
                                     P.flags = flags
                                   }
    where --Subqueries to get build targets
          --Get global library from a packag key wrapped in a maybe as not needed to exist in package
          getGlobalLibrary packageKey 
              = do libraryEntityM <- getBy $ GlobalLibraryUniquePackage packageKey
                   case libraryEntityM of
                      Nothing -> return Nothing
                      Just libraryEntity -> 
                             do dependenceEntities <- selectList [GlobalLibraryDependencyLibrary ==. entityKey libraryEntity] []
                                let dependences = map (fromLibraryDep . entityVal) dependenceEntities
                                return $ Just P.Library {
                                  P.libraryBuildDependencies = dependences
                                }
          --Get executable from a global executable entity
          getGlobalExecutable executableEntity
              = do dependenceEntities <- selectList [GlobalExecutableDependencyExecutable ==. entityKey executableEntity] []
                   let dependences = map (fromExecutableDep . entityVal) dependenceEntities
                   return P.Executable {
                     P.executableTargetName = globalExecutableName $ entityVal executableEntity,
                     P.executableBuildDependencies = dependences
                   }
          --Get test from a global test entity
          getGlobalTest testEntity
              = do dependenceEntities <- selectList [GlobalTestDependencyTest ==. entityKey testEntity] []
                   let dependences = map (fromTestDep . entityVal) dependenceEntities
                   return P.TestSuite {
                     P.testTargetName = globalTestName $ entityVal testEntity,
                     P.testBuildDependencies = dependences
                   }
          --Get benchmark from a global benchmark entity
          getGlobalBenchmark benchmarkEntity
              = do dependenceEntities <- selectList [GlobalBenchmarkDependencyBenchmark ==. entityKey benchmarkEntity] []
                   let dependences = map (fromBenchmarkDep . entityVal) dependenceEntities
                   return P.Benchmark {
                     P.benchmarkTargetName = globalBenchmarkName $ entityVal benchmarkEntity,
                     P.benchmarkBuildDependencies = dependences
                   }

--Query for inserting global package
addGlobalPackage globalPackage =
    do --Seperate package identifier from name and version
       let name = P.name $ P.globalProperties globalPackage
           version = P.version $ P.globalProperties globalPackage
           globalData = fromGlobalPackageDataModel $ P.globalProperties globalPackage
       --Insert global package data
       globalDataId <- insert globalData
       packageId <- insert $ GlobalPackage name version globalDataId
       --if there is library insert it
       case (P.library globalPackage) of
           Nothing -> return ()
           Just library -> insertGlobalLibrary packageId library
       
       return ()
    where --Insert a global library from cabal library
          --Incude ref to the parent global package
          --Then insert dependencies
          insertGlobalLibrary packageId library = 
                  do libraryId <- insert $ GlobalLibrary packageId
                     let libraryDeps = map (toLibraryDep libraryId) $ P.libraryBuildDependencies library
                     insertMany libraryDeps
                     return () --Fit null return type.
          --Insert a global executable from cabal executable
          --Incude ref to the parent global package
          --Then insert dependencies
          insertGlobalExecutable packageId executable = 
                  do let name = P.executableTargetName executable
                     executableId <- insert $ GlobalExecutable name packageId
                     let executableDeps = map (toExecutableDep executableId) $ P.executableBuildDependencies executable
                     insertMany executableDeps
          --Insert a global test from cabal test
          --Incude ref to the parent global package
          --Then insert dependencies
          insertGlobalTest packageId test = 
                  do let name = P.testTargetName test
                     testId <- insert $ GlobalTest name packageId
                     let testDeps = map (toTestDep testId) $ P.testBuildDependencies test
                     insertMany testDeps
          --Insert a global benchmark from cabal benchmark
          --Incude ref to the parent global package
          --Then insert dependencies
          insertBenchamrkExecutable packageId benchmark = 
                  do let name = P.benchmarkTargetName benchmark
                     benchmarkId <- insert $ GlobalBenchmark name packageId
                     let benchmarkDeps = map (toBenchmarkDep benchmarkId) $ P.benchmarkBuildDependencies benchmark
                     insertMany benchmarkDeps