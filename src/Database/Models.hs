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


--Construct the database models for the data structures used in the cabal models
--Add interfaces between them


mkPersist sqlSettings [persistLowerCase|
GlobalPackageData
    name String
    version String
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
|]


--Convert to/from the cabal modules

fromGlobalPackageDataModel :: P.GlobalPackageData -> GlobalPackageData
fromGlobalPackageDataModel globalPackageData = GlobalPackageData {
  globalPackageDataName = show $ P.name globalPackageData,
  globalPackageDataVersion = show $ P.version globalPackageData,
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
  P.name = read $ globalPackageDataName globalPackageDataModel,
  P.version = read $ globalPackageDataVersion globalPackageDataModel,
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
