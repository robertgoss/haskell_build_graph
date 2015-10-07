module Package where

import Distribution.Version(VersionRange)

import Data.Map(Map)

import Conditional

--A series of data structures to represent a cabal package for building
--We want to seperate the (pure) conditional package (etc) from the one which
--is platform dependent. We use our own class here to clear up these destinctions
-- Unlike in the cabal data structures. 
--As we would like to have a database with information common to all
--Also it helps us to have control over the data structures when using persistence.


--A package is given by it's global data and the various build targets which are kept at the conditional level
--As there are differing types of conditioning levels depending on if the result is global, on a package or resolved with all flags
data PackageConditional conditional = Package {
  globalProperties :: GlobalPackageData, --The global properties of the package.
  --BuildTargets
  library :: Maybe (Library conditional), -- Optional Conditional library build target.
  executables :: [Executable conditional], -- Executable build targets kept in conditional form.
  tests :: [TestSuite conditional], -- Executable build targets kept in conditional form.
  benchmarks :: [Benchmark conditional], -- Executable build targets kept in conditional form.
  --Flags the various flags availible and their deafault level
  flags :: [ConfigurationFlag]
}

--Types of packages with different levels of conditionality.
type Package = PackageFlagged -- A type of package to type and build.
type PackageFlagged = PackageConditional Bool --All conditionals have been resolved and flags have been set.
type PackagePlatform = PackageConditional FlagConditional -- The package has been resolved to a platform only flags need to be set.
type PackageGlobal = PackageConditional PlatformConditional -- The package has had no conditionals resolved is it not specifed to a platform.


--Helper types
-- Some helper structured types for fields
-- These add readability to descriptors
-- They may be exteneded to newtypes to add validation
-- For now we just use synonyms
type Filename = String -- The system address of a file
type WildcardFilename = String --The a pattern for the system address of some files
                               -- must be in the form of dir/*.extension with no other wildcard.
type URL = String --A web url address.
type Version = [Int] -- A hiarachical version number red right to left.
type LicenseType = String -- The type of license used.
type PackageName = String --The name of a package without version information 

--Represent the global package data from the
--Not specific to any particular platform or build.
data GlobalPackageData = GlobalPackageData {
    --Global identifiers of this package.
    name :: PackageName, -- The name of the package without version number
    version :: Version, --The version number of the package
    --Global package properties optional
      --Package description
    synopsis :: Maybe String, --Optional, short description of the package.
    description :: Maybe String, --Optional, description of the package.
    category :: Maybe String, --Optional, classification of the package with hackage category.
    stability :: Maybe String, --Optional, the stability of the package alpha, beta etc.
      --License information
    license :: Maybe LicenseType, -- Optional, type of the license this package uses.
    licenseFiles :: [Filename], -- List of files for licenses, can be empty.
    copyright :: Maybe String, -- Optional, copyright notice.
      --People and addresses.
    author :: Maybe String, -- Optional, original author of the package.
    maintainer :: Maybe String, -- Optional, email address of current package maintainer.
    homepage :: Maybe URL, --Optional, homepage of the package.
    bugReports :: Maybe URL, --Optional, webaddress to send bug reports to.
    packageUrl :: Maybe URL --Optional, webaddress source of the package.
}

--A data structure to represent a configuration flag
data ConfigurationFlag = ConfigurationFlag {
     flagName :: String, -- The name of to identify this flag.
     flagDefault :: Bool, -- Does this flag default to true or false.
     flagDescription :: String, --A description of this flag.
     manual :: Bool --If this flag can be negated from the default in dependence resolution.
}

--A helper type
--Represents a build dependency is a pair of package and a version constraint
type PackageDependency = (PackageName, VersionRange)

--A data structure to represent a cabal libray with fields wrapped in conditionals
-- These conditionals may be trivial (ie boolean true).
data Library conditional = Library {
  libraryBuildDependencies :: [(conditional,PackageDependency)] --The build dependencies of this library dependent on the platform and flags
}

--A data structure to represent a cabal executable with fields wrapped in conditionals
-- These conditionals may be trivial (ie boolean true).
data Executable conditional = Executable {
  executableTargetName :: String, --The name of this executable target
  executableBuildDependencies :: [(conditional,PackageDependency)] --The build dependencies of this executable dependent on the platform and flags
}

--A data structure to represent a cabal test suite with fields wrapped in conditionals
-- These conditionals may be trivial (ie boolean true).
data TestSuite conditional = TestSuite {
  testTargetName :: String, --The name of this test target
  testBuildDependencies :: [(conditional,PackageDependency)] --The build dependencies of this test dependent on the platform and flags
}

--A data structure to represent a cabal benchmark with fields wrapped in conditionals
-- These conditionals may be trivial (ie boolean true).
data Benchmark conditional = Benchmark {
  benchmarkTargetName :: String, --The name of this benchmark target
  benchmarkBuildDependencies :: [(conditional,PackageDependency)] --The build dependencies of this benchmark dependent on the platform and flags
}
