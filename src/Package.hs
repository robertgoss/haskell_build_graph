module Package where

import Data.Map(Map)

--A series of data structures to represent a cabal package for building
--
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


--Represent the full package from the file.
--Not specified down to any particular platform.
data Package = Package {
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
    packageUrl :: Maybe URL, --Optional, webaddress source of the package.
      --Global building information
    cabalVersion :: Maybe [Int], --Optional, the minimum version of cabal this package needs to build.
    testedWith :: [CompilerVersion], --Compiler versions that this package has been built with.
    dataFiles :: [WildcardFilename], --Static data files to be copied over on the install. Limited wildcards allowed.
    dataDir :: Maybe Filename, --Data directory that cabal will look for data files in.
                               --If not set defaults to source directory
    extraSourceFiles :: [WildcardFilename], --Additional source files to include.
    extraDocFiles :: [WildcardFilename], -- Additional doc files to include.
    extraTmpFiles :: [WildcardFilename], --Additional tmp files to include.
    --
    --Configuration Flags
    configurationFlags :: [ConfigurationFlag], --Configuration flags that can appear in the library or executable  
    --Libraries
    --
    libraries :: Maybe LibraryConditional, --The optional library still in conditional form
                                           --Ie the conditionals have not been reduced for the build system.
    --Executables
    --
    executables :: [ExecutableConditional], --The avalible executables still in executable form
                                            --Ie the conditionals have not been reduced for the build system.
    --Tests
    tests :: [TestConditional], --Any test suites present.
    
    --Benchmarks
    benchmarks :: [BenchmarkConditional] --Any benchmarks present.
}

--A data structure to represent the compilier version
data Compiler = GHC | JHC | UHC | LHC --The valid haskell compiler flavours
data CompilerVersion = CompilerVersion Compiler VersionConstraint -- The compiler and a version range

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
--or one of 2 values is returned dependdent on the evaluation of the conditional
data Conditional a = Conditional ConditionalType a
                   | ConditionalEither ConditionalType a a
--The folowing conditional is a restriction of the one above and must always return a value
--It is for values that are conditional but required.
data RequiredConditional a = RequiredConditional ConditionalType a a

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
  libraryBuildInformation :: BuildInformationConditional, -- The build information for this library with conditionals
  exposed :: Conditional Bool, --If we are using the ghc exposed module setting when other packages import this one
  exposedModules :: [Conditional ModuleName], --The modules this library exposes to be imported by other packages.
                                              --Required to be non-empty
  reexportedModules :: [Conditional ReexportPath] --Modules from dependencies that this module should re export
                                                  --And the name to export them under.
}

--A data structure to represent a cabal executable with fields wrapped in conditionals
-- These conditionals may be trivial (ie boolean true).
data ExecutableConditional = ExecutableConditional {
  executableBuildInformation :: BuildInformationConditional, -- The build information for this executable with conditionals
  mainIs :: RequiredConditional ModuleName --The main module to run the executable from.
}

--A data structure to represent the data in a test runner
data TestRunner = TestRunnerExitcode ModuleName --Runs the test as an executable and determines success based on the 
                                                --process exitcode. Need to be supplied with a main module.
                | TestRunnerDetailed ModuleName --Runs a test in a framework allowing case by case failure.
                                                --Supplied with a module that exports tests :: IO [Test].

--A data structure to represent a cabal test with fields wrapped in conditionals
-- These conditionals may be trivial (ie boolean true).
data TestConditional = TestConditional {
  testBuildInformation :: BuildInformationConditional, -- The build information for this test with conditionals
  testRunner :: RequiredConditional TestRunner --The type of the test runner used
                                               --One must bew specified
}

--A data structure to represent the data in a benchmark runner
data BenchmarkRunner = BenchmarkRunnerExitcode ModuleName --Runs the test as an executable and determines success based on the 
                                                          --process exitcode. Need to be supplied with a main module.

--A data structure to represent a cabal benchmark with fields wrapped in conditionals
-- These conditionals may be trivial (ie boolean true).
data BenchmarkConditional = BenchmarkConditional {
  benchmarkBuildInformation :: BuildInformationConditional, -- The build information for this benchmark with conditionals
  benchmarkRunner :: RequiredConditional BenchmarkRunner --The type of the benchmark runner used
                                                         --One must bew specified
}

--A helper type
--Represents a build dependency is a pair of package and a version constraint
type PackageConstraint = (PackageName, VersionConstraint)

--A data structor to represent a build dependency
--Combines a package constraint with optional module thining
data PackageDependency = PackageDependency PackageConstraint
                       --Hide all modules but those appearing in the list
                       --And renamed modules in the map.
                       | PackageDependencyWithHiding PackageConstraint [ModuleName] (Map ModuleName ModuleName)
                        --add additional names for modules with the second map
                       | PackageDependencyWithRenaming PackageConstraint (Map ModuleName ModuleName)
--Compiler extensions 
type Extension = String

--The name of a tool - a binary program used during build
type ToolName = String

--A data structor to represent a tool dependency
--Combines a tool name with version constraint 
data ToolDependency = ToolDependency ToolName VersionConstraint

--An external library to link to
type ExternalLibrary = String
--A package name for the pkgconfig tool
type PkgconfigPackage = String

--A data structure to represent the build information for a build with fields wrapped in conditionals
-- These conditionals may be trivial (ie boolean true).
data BuildInformationConditional = BuildInformationConditional {
  buildDependencies :: [Conditional PackageDependency], --The constriant on package versions required 
                                                        --to be installed to build this package.
  otherModules :: [Conditional ModuleName], --Other modules to be built but not exposed or used in output.
  hsSourceDirs :: [Conditional Filename], --Source root-directories for the build to find modules.
  extensions :: [Conditional Extension], --A list of extensions to pass to the compiler.
  buildTools :: [Conditional ToolDependency], --A list of the binry tools - with versions used in build.
  buildable :: Conditional Bool, --If this build is possible - more used with custom/config builds.
  ghcOptions :: [Conditional String], --Additional ghc options.
  ghcProfOptions :: [Conditional String], --Additional ghc options when building with profiling.
  ghcSharedOptions :: [Conditional String], --Additional ghc options when building as shared.
  headerIncludes :: [Conditional Filename], --Headers to be included in the build.
  headerIncludeInstalls :: [Conditional Filename], --Headers to be installed in the outcome.
  headerIncludeDirs :: [Conditional Filename], --Directories to be searched for headers.
  cSources :: [Conditional Filename], -- C source files to be compiled with the build.
  jsSources :: [Conditional Filename], -- javascript source files to be compiled with the build.
  extraLibraries :: [Conditional ExternalLibrary], --Extra libraries to link into as part of build
  extraGHCILibraries :: [Conditional ExternalLibrary], --Extra libraries to link into when running ghci
  extraLibraryDirs :: [Conditional Filename], --Directories to be searched for libraries.
  ccOptions :: [Conditional String], --Options passed to c compiler.
  cppOptions :: [Conditional String], --Options passed to c++ compiler.
  ldOptions :: [Conditional String], --Options passed to linker.
  pkgconfigDepends :: [Conditional PkgconfigPackage], --Package config dependencies.
  buildFrameworks :: [Conditional String] -- On mac build frameworks to link to.
}