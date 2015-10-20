module Cabal.Platform where

import Cabal.Package(GlobalPackage,PlatformPackage,
                     PackageConditional(..),
                     Library(..),
                     Executable(..),
                     TestSuite(..),
                     Benchmark(..))

import Distribution.System(OS,Arch)
import Cabal.Conditional(CompilerVersion
                        ,PlatformBasicConditional(..)
                        ,PlatformConditional
                        ,PlatformConditionalType
                        ,FlagConditional
                        ,unwrapPlatformConditionalType,
                        ConditionalTree(..), simplifyTree, changeVars)
import Control.Applicative ((<$>))

--A data structure to represent a given platform that packages
-- Can be configured and built on.
--Includes the basic tests that can be performed in a cabal file

data Platform = Platform {
  operatingSystem :: OS,
  architecture :: Arch,
  compiler :: CompilerVersion,
  globalPlatformName :: String -- A globally unique name for this build platform.
} 

--Localise a platform conditional by 
--Substituting the plaform conditions in with the given platform
--Then simplify the resultant tree
localiseConditional :: Platform -> PlatformConditional -> FlagConditional
localiseConditional platform condTree = simplifyTree $ changeVars localiseVars condTree
  where localiseVars (Right flagVar) = Var flagVar
        localiseVars (Left (OperatingSystem os)) = Logic (os == operatingSystem platform)
        localiseVars (Left (Architecture arch)) = Logic (arch == architecture platform)
        

localiseConditionalType :: Platform -> PlatformConditionalType -> FlagConditional
localiseConditionalType platform = localiseConditional platform . unwrapPlatformConditionalType

localisePackage :: Platform -> GlobalPackage -> PlatformPackage
localisePackage platform globalPackage = Package{
  globalProperties = globalProperties globalPackage,
  library = localiseLibrary <$> library globalPackage,
  executables = map localiseExecutable $ executables globalPackage,
  tests = map localiseTest $ tests globalPackage,
  benchmarks = map localiseBenchmark $ benchmarks globalPackage,
  flags = flags globalPackage
}
   where localiseLibrary globalLibrary = Library {
                                           libraryBuildDependencies = map localDep $ libraryBuildDependencies globalLibrary
                                         }
         localiseExecutable globalExe = Executable {
                                           executableTargetName = executableTargetName globalExe,
                                           executableBuildDependencies = map localDep $ executableBuildDependencies globalExe
                                        }
         localiseTest globalTest = TestSuite {
                                      testTargetName = testTargetName globalTest,
                                      testBuildDependencies = map localDep $ testBuildDependencies globalTest
                                   }
         localiseBenchmark globalBen = Benchmark {
                                         benchmarkTargetName = benchmarkTargetName globalBen,
                                         benchmarkBuildDependencies = map localDep $ benchmarkBuildDependencies globalBen
                                       }
         localDep (cond,dep) = (localiseConditional platform cond,dep)