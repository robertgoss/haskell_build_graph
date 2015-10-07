module Package where

import Data.Map(Map)

--A series of data structures to represent a cabal package for building
--We want to seperate the (pure) conditional package (etc) from the one which
--is platform dependent. We use our own class here to clear up these destinctions
-- Unlike in the cabal data structures. 
--As we would like to have a database with information common to all
--Also it helps us to have control over the data structures when using persistence.


--A package is given by it's global data and the various build targets which are kept at the conditional level
data PackageConditional = PackageConditional {
  globalProperties :: GlobalPackageData, --The global properties of the package.
  --BuildTargets
  library :: Maybe LibraryConditional, -- Optional Conditional library build target.
  executables :: [ExecutableConditional], -- Executable build targets kept in conditional form.
  tests :: [TestConditional], -- Executable build targets kept in conditional form.
  benchmarks :: [BenchmarkConditional] -- Executable build targets kept in conditional form.
}


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

--A data structure to represent the compilier version
data Compiler = GHC | JHC | UHC | LHC --The valid haskell compiler flavours
data CompilerVersion = CompilerVersion Compiler VersionConstraint -- The compiler and a version range

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

--Conditional package dependencies for each of the build targets.

--A data structure to represent version constraint.
data VersionConstraint = NoConstraint --No constraint on the version
                       | Exact Version --An exact version 
                       | LessThan Version --Less than a given version number
                       | GreaterThan Version --Greater than a given version number
                       | Intersect VersionConstraint VersionConstraint --Bth version constraints need to hold
                       | Union VersionConstraint VersionConstraint --Either version constraint can hold.

--A data stucture to hold the conditionals that cabal can have
data ConditionalType = OpertionSystem String -- Tests if the platform operating system is the given string
                     | Architecture String --Tests if the platform architecture is the given string
                     | Implimentaton CompilerVersion --Tests the platform compiler version
                     | Flag String --Tests the setting of the given flag on the given platform
                     | Logic Bool --Returns the given boolean
                     | And ConditionalType ConditionalType --If both conditionals hold
                     | Or ConditionalType ConditionalType--If either conditional hold
                     | Not ConditionalType --If the conditional does not hold
                     
--A value wrapped with an assosiated conditional
--A value is either included or not dependent on the evaluation of the conditional
data Conditional a = Conditional ConditionalType a


--A data structure to represent a configuration flag
data ConfigurationFlag = ConfigurationFlag {
     flagName :: String, -- The name of to identify this flag.
     flagDefault :: Bool, -- Does this flag default to true or false.
     flagDescription :: String, --A description of this flag.
     manual :: Bool --If this flag can be negated from the default in dependence resolution.
}

--Some helper type synonyms
type ModuleName = [String] --Full path module name.
type ReexportPath = (PackageName, ModuleName, ModuleName) --A triple of the the package to reexport from
                                                          --The original module name
                                                          --The new name to expose the module as.

--A data structure to represent a cabal libray with fields wrapped in conditionals
-- These conditionals may be trivial (ie boolean true).
data LibraryConditional = LibraryConditional {
  libraryBuildDependencies :: [Conditional PackageDependency] --The build dependencies of this library dependent on the platform and flags
}

--A data structure to represent a cabal executable with fields wrapped in conditionals
-- These conditionals may be trivial (ie boolean true).
data ExecutableConditional = ExecutableConditional {
  executableTargetName :: String, --The name of this executable target
  executableBuildDependencies :: [Conditional PackageDependency] --The build dependencies of this executable dependent on the platform and flags
}

--A data structure to represent a cabal test suite with fields wrapped in conditionals
-- These conditionals may be trivial (ie boolean true).
data TestConditional = TestConditional {
  testTargetName :: String, --The name of this test target
  testBuildDependencies :: [Conditional PackageDependency] --The build dependencies of this test dependent on the platform and flags
}

--A data structure to represent a cabal benchmark with fields wrapped in conditionals
-- These conditionals may be trivial (ie boolean true).
data BenchmarkConditional = BenchmarkConditional {
  benchmarkTargetName :: String, --The name of this benchmark target
  benchmarkBuildDependencies :: [Conditional PackageDependency] --The build dependencies of this benchmark dependent on the platform and flags
}

--A helper type
--Represents a build dependency is a pair of package and a version constraint
type PackageDependency = (PackageName, VersionConstraint)