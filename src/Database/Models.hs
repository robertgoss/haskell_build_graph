{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Database.Models where

--The database models for the cabal data

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)

import qualified Cabal.Package as P
import qualified Database.Fields as Field(Version,PackageName,VersionRange,PlatformConditionalType)


--Construct the database models for the data structures used in the cabal models
--Add interfaces between them

mkPersist sqlSettings [persistLowerCase|
--Global package data a gloss over the package version
GlobalPackageData
    name Field.PackageName
    version Field.Version
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
    packageData GlobalPackageData --Associated package data
--A global version of a library with no platform localisation or flags set.
--Has inverse maps for dependencies
GlobalLibrary
    package GlobalPackage
GlobalLibraryDependency
    library GlobalLibrary
    condition Field.PlatformConditionalType
    dependance Field.VersionRange
--A global version of a executable with no platform localisation or flags set.
--Has inverse maps for dependencies
GlobalExecutable
    name String
    package GlobalPackage
GlobalExecutableDependency
    executable GlobalExecutable
    condition Field.PlatformConditionalType
    dependance Field.VersionRange
--A global version of a test suite with no platform localisation or flags set.
--Has inverse maps for dependencies
GlobalTest
    name String
    package GlobalPackage
GlobalTestDependency
    test GlobalTest
    condition Field.PlatformConditionalType
    dependance Field.VersionRange
--A global version of a benchmark with no platform localisation or flags set.
--Has inverse maps for dependencies
GlobalBenchmark
    name String
    package GlobalPackage
GlobalBenchmarkDependency
    benchmark GlobalBenchmark
    condition Field.PlatformConditionalType
    dependance Field.VersionRange
|]

--


--Convert to/from the cabal modules

fromGlobalPackageDataModel :: P.GlobalPackageData -> GlobalPackageData
fromGlobalPackageDataModel globalPackageData = GlobalPackageData {
  globalPackageDataName = P.name globalPackageData,
  globalPackageDataVersion = P.version globalPackageData,
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

toGlobalPackageDataModel :: GlobalPackageData -> P.GlobalPackageData
toGlobalPackageDataModel globalPackageDataModel = P.GlobalPackageData {
  P.name = globalPackageDataName globalPackageDataModel,
  P.version = globalPackageDataVersion globalPackageDataModel,
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
